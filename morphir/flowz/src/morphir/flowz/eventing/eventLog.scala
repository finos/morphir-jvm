package morphir.flowz.eventing
import zio._
import zio.prelude._
import zio.stm._
import zio.stream._

object eventLog {
  object EventStreamId extends Newtype[String]
  type EventStreamId = EventStreamId.Type

  type EventLog[Event] = Has[EventLog.Service[Event]]
  object EventLog {
    trait Service[Event] {
      def persistEvent(streamId: EventStreamId, event: Event): Task[Unit]
      def loadEvents(streamId: EventStreamId): Stream[Throwable, Event]
      def allIds: Stream[Throwable, EventStreamId]
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
