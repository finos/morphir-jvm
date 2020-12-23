package morphir.flowz.eventing
import morphir.flowz.eventing.behavior.AggregateBehavior
import zio._
import zio.stm._
import zio.stream._

object eventLog {

  def aggregate[E, S](initial: S)(update: (S, E) => Task[S]): Task[AggregateBehavior[E, S]] =
    ZIO.succeed(new AggregateBehavior(initial, update))

  def allIds[Event: Tag]: ZStream[EventLog[Event], Throwable, EventStreamId] =
    ZStream.accessStream[EventLog[Event]](_.get.allIds)

  def allAggregates[Event: Tag, State](
    behavior: AggregateBehavior[Event, State]
  ): ZStream[EventLog[Event], Throwable, Aggregate[Event, State]] =
    ZStream.accessStream[EventLog[Event]](_.get.allAggregates(behavior))

  def createAggregate[Event: Tag, State](
    streamId: EventStreamId,
    behavior: AggregateBehavior[Event, State]
  ): ZIO[EventLog[Event], Throwable, Aggregate[Event, State]] =
    ZIO.accessM[EventLog[Event]](_.get.createAggregate(streamId, behavior))

  def load[Event: Tag, State](
    streamId: EventStreamId,
    behavior: AggregateBehavior[Event, State]
  ): ZIO[EventLog[Event], Throwable, Aggregate[Event, State]] =
    ZIO.accessM[EventLog[Event]](_.get.load(streamId, behavior))

  def loadEvents[Event: Tag](streamId: EventStreamId): ZStream[EventLog[Event], Throwable, Event] =
    ZStream.accessStream[EventLog[Event]](_.get.loadEvents(streamId))

  def persistEvent[Event: Tag](streamId: EventStreamId, event: Event): RIO[EventLog[Event], Unit] =
    ZIO.accessM(_.get.persistEvent(streamId, event))

  type EventLog[Event] = Has[EventLog.Service[Event]]
  object EventLog {
    trait Service[Event] {

      def allIds: Stream[Throwable, EventStreamId]

      /**
       * Stream of all aggregates stored
       */
      def allAggregates[State](behavior: AggregateBehavior[Event, State]): Stream[Throwable, Aggregate[Event, State]] =
        allIds.mapM(load(_, behavior))

      /**
       * Create new empty aggregate
       */
      def createAggregate[State](
        streamId: EventStreamId,
        behavior: AggregateBehavior[Event, State]
      ): Task[Aggregate[Event, State]] =
        for {
          initialStateRef <- Ref.make(behavior.initialState)
        } yield new Aggregate[Event, State](streamId, initialStateRef, behavior.update, persistEvent)

      def loadEvents(streamId: EventStreamId): Stream[Throwable, Event]

      /**
       * Load aggregate from event journal
       */
      def load[State](
        streamId: EventStreamId,
        behavior: AggregateBehavior[Event, State]
      ): Task[Aggregate[Event, State]] =
        for {
          agg <- createAggregate[State](streamId, behavior)
          res <- loadEvents(streamId).foldM(agg)(_ appendNoPersist _)
        } yield res

      def persistEvent(streamId: EventStreamId, event: Event): Task[Unit]
    }

    object Service {

      def inMemory[Event]: UIO[EventLog.Service[Event]] =
        TMap.empty[EventStreamId, Chunk[Event]].commit.map(new InMemory[Event](_))

      private class InMemory[Event](private val store: TMap[EventStreamId, Chunk[Event]])
          extends EventLog.Service[Event] {

        private def getEventsFor(stream: EventStreamId): USTM[Chunk[Event]] =
          store.get(stream).flatMap {
            case Some(events) => STM.succeed(events)
            case None         => STM.succeed(Chunk.empty).flatMap(events => store.put(stream, events).as(events))
          }

        private def updateEventsFor(stream: EventStreamId, events: Chunk[Event]): USTM[Unit] =
          store.put(stream, events)

        def persistEvent(streamId: EventStreamId, event: Event): Task[Unit] =
          (for {
            events <- getEventsFor(streamId)
            _      <- updateEventsFor(streamId, events :+ event)
          } yield ()).commit

        def loadEvents(streamId: EventStreamId): Stream[Throwable, Event] =
          Stream.fromIteratorEffect(getEventsFor(streamId).commit.map(_.iterator))

        def allIds: Stream[Throwable, EventStreamId] =
          Stream.unwrap(store.keys.commit.map(ids => Stream.fromIterable(ids)))
      }
    }

    def any[Event: Tag]: ZLayer[EventLog[Event], Nothing, EventLog[Event]] = ZLayer.requires[EventLog[Event]]
    def inMemory[Event: Tag]: ULayer[EventLog[Event]]                      = Service.inMemory[Event].toLayer
  }
}
