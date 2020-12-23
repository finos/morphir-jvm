package morphir.flowz

import morphir.flowz.eventPublisher.InMemoryEventPublisher.{ EventHandlerRegistry, Sequence }
import zio.console.Console
import zio.{ ExitCode, URIO, ZIO }
import zio.test._
import zio.test.Assertion._

object EventPublisherSpec extends DefaultRunnableSpec {
  import MyEvent._
  def spec = suite("EventPublisher Spec")(
    suite("Sequence Spec")(
      testM("Sequence should allow you to get the next values")(
        for {
          result <- (for {
                      sequence <- Sequence.make()
                      first    <- sequence.next
                      second   <- sequence.next
                      third    <- sequence.next

                    } yield List(first, second, third)).commit
        } yield assert(result)(equalTo(List(1L, 2L, 3L)))
      ),
      testM("Sequence should allow you to access the current value without incrementing")(
        for {
          result <- (for {
                      sequence <- Sequence.make()
                      first    <- sequence.next
                      second   <- sequence.current
                      third    <- sequence.current

                    } yield List(first, second, third)).commit
        } yield assert(result)(equalTo(List(1L, 1L, 1L)))
      )
    ),
    suite("EventHandlerRegistry Spec")(
      testM("It should allow adding event handlers") {
        val handler1 = (foo: MyEvent.FooEvent) => ZIO.succeed(s"Foo: $foo")
        val handler2 = (foo: MyEvent.FooEvent) => ZIO.succeed(s"[Foo]: $foo")
        for {
          actual <- (for {
                      registry <- EventHandlerRegistry.make[FooEvent]
                      _        <- registry.addHandler(handler1)
                      _        <- registry.addHandler(handler2)
                      handlers <- registry.getHandlers
                    } yield handlers).commit
        } yield assert(actual)(hasSameElements(List(handler1, handler2)))
      },
      testM("It should remove event handlers when unsubscribe is called") {
        val handler1 = (foo: MyEvent) => ZIO.succeed(s"Handler1: $foo")
        val handler2 = (foo: MyEvent) => ZIO.succeed(s"Handler2: $foo")
        val handler3 = (foo: MyEvent) => ZIO.succeed(s"Handler3: $foo")
        for {
          results <- (for {
                       registry <- EventHandlerRegistry.make[FooEvent]
                       _        <- registry.addHandler(handler1)
                       sub      <- registry.addHandler(handler2)
                       _        <- registry.addHandler(handler3)
                       _        <- registry.getHandlers
                     } yield (registry, sub)).commit
          registry     = results._1
          subscription = results._2
          _           <- subscription.unsubscribe()
          actual      <- registry.getHandlers.commit
        } yield assert(actual)(hasSameElements(List(handler3, handler1)))
      }
    )
  )

  sealed trait MyEvent
  object MyEvent {
    final case class FooEvent() extends MyEvent
    final case class BarEvent() extends MyEvent
  }
}

object eventPublisher {
  import zio._
  import InMemoryEventPublisher._

  type EventPublisher = Has[EventPublisher.Service]
  object EventPublisher {
    trait Service {
      def publish[Event: Tag](event: Event): Task[Unit]
      def subscribe[Event: Tag](
        handler: EventHandler[Event]
      ): Task[EventSubscription[Event]]
    }
  }

  trait EventSubscription[Event] {
    def unsubscribe(): UIO[Any]
  }

  type EventHandler[-Event] = Event => UIO[Any]

  final case class InMemoryEventPublisher(
    private val queue: Queue[EventPublication[_]],
    private val subscriptions: SubscriptionsRegistry
  ) extends eventPublisher.EventPublisher.Service { self =>

    def run: ZManaged[Any, Nothing, EventPublisher.Service] =
      for {
        _ <- queue.take.flatMap(publication => publication.run).forever.forkManaged
        _ <- ZManaged.succeed(println("Run exited"))
      } yield self

    def publish[Event: zio.Tag](event: Event): Task[Unit] =
      for {
        subscribedEvents <- subscriptions.getAllHandlers[Event].commit
        publication       = EventPublication(event, subscribedEvents)
        _                <- queue.offer(publication).fork
      } yield ()

    def subscribe[Event: zio.Tag](handler: EventHandler[Event]): Task[eventPublisher.EventSubscription[Event]] =
      subscriptions.subscribe(handler).commit

  }
  object InMemoryEventPublisher {
    import zio._
    import zio.stm._

    def make: UIO[InMemoryEventPublisher] =
      for {
        queue         <- Queue.bounded[EventPublication[_]](100)
        subscriptions <- SubscriptionsRegistry.makeCommit
      } yield InMemoryEventPublisher(queue, subscriptions)

    final case class EventPublication[Event: Tag](
      event: Event,
      handlers: List[EventHandler[Event]]
    ) {
      def run: ZIO[Any, Nothing, List[Any]] =
        ZIO.foreachPar(handlers)(handler => handler.apply(event).fork)
    }
    final case class SubscriptionsRegistry(
      registries: TMap[LightTypeTag, EventHandlerRegistry[_]]
    ) {

      def subscribe[Event](
        handler: EventHandler[Event]
      )(implicit
        eventTypeTag: Tag[Event]
      ): ZSTM[Any, Nothing, EventSubscription[Event]] =
        for {
          registry     <- getOrCreateRegistry[Event]
          subscription <- registry.addHandler(handler)
        } yield subscription

      def getAllHandlers[Event](implicit
        eventTypeTag: Tag[Event]
      ): STM[Nothing, List[EventHandler[Event]]] = {
        val eventType = eventTypeTag.tag

        registries.foldM(List.empty[EventHandler[Event]]) {
          case (handlers, (subscribedEventType, registry))
              if eventType =:= subscribedEventType || eventType <:< subscribedEventType =>
            registry.getHandlers.map(applicableHandlers =>
              handlers ++ applicableHandlers.asInstanceOf[List[EventHandler[Event]]]
            )
          case (handlers, (_, registry)) =>
            registry.getHandlers.map(_ => handlers)
        }
      }

      private def getOrCreateRegistry[Event](implicit
        eventTypeTag: Tag[Event]
      ): ZSTM[Any, Nothing, EventHandlerRegistry[Event]] =
        registries.get(eventTypeTag.tag).flatMap {
          case Some(registry) =>
            STM.succeed(registry.asInstanceOf[EventHandlerRegistry[Event]])
          case None =>
            EventHandlerRegistry
              .make[Event]
              .flatMap(v => registries.put(eventTypeTag.tag, v).as(v))
        }
    }

    object SubscriptionsRegistry {
      def make: ZSTM[Any, Nothing, SubscriptionsRegistry] =
        TMap.empty[LightTypeTag, EventHandlerRegistry[_]].map(SubscriptionsRegistry(_))

      def makeCommit: UIO[SubscriptionsRegistry] = make.commit
    }

    final case class EventHandlerRegistry[Event: Tag] private (
      private val entries: TMap[Long, EventHandler[Event]],
      sequence: Sequence
    ) { self =>
      lazy val eventTag: Tag[Event]    = Tag[Event]
      lazy val eventType: LightTypeTag = eventTag.tag

      def addHandler(
        handler: EventHandler[Event]
      ): ZSTM[Any, Nothing, EventSubscription[Event]] =
        (for {
          myId <- sequence.next
          subscription = new EventSubscription[Event] {
                           def unsubscribe(): UIO[Any] = entries.delete(myId).commit
                         }
          _ <- entries.put(myId, handler)
        } yield subscription)

      def getHandlers: USTM[List[EventHandler[Event]]] = entries.values

      def canHandle[That](implicit thatEventTypeTag: Tag[That]): USTM[Boolean] =
        ZSTM.succeed {

          val thatEventType = thatEventTypeTag.tag
          (thatEventType =:= eventType) || (eventType <:< thatEventType)
        }
    }

    object EventHandlerRegistry {
      def make[Event: Tag]: ZSTM[Any, Nothing, EventHandlerRegistry[Event]] =
        for {
          handlers <- TMap.empty[Long, EventHandler[Event]]
          sequence <- Sequence.make()
        } yield EventHandlerRegistry(handlers, sequence)

      def makeCommit[Event: Tag]: ZIO[Any, Nothing, EventHandlerRegistry[Event]] =
        make[Event].commit
    }

    final case class Sequence(private val value: TRef[Long]) {
      def current: STM[Nothing, Long] = value.get
      def next: STM[Nothing, Long] =
        value.updateAndGet(currentVal => currentVal + 1)
    }

    object Sequence {
      def make(initialValue: Long = 0): ZSTM[Any, Nothing, Sequence] =
        TRef.make(initialValue).map(Sequence(_))
      def makeCommit(initialValue: Long = 0): ZIO[Any, Nothing, Sequence] = make(
        initialValue
      ).commit
    }
  }
}

import zio._
object Demo extends App {
  import MyEvent._
  def run(args: List[String]): URIO[Any with Console, ExitCode] = (
    for {
      publisher <- eventPublisher.InMemoryEventPublisher.make
      _ <- publisher.subscribe { event: FooEvent =>
             console.putStrLn(s"Received FooEvent: $event").provide(environment)
           }
      _ <- publisher.subscribe { event: BarEvent =>
             console.putStrLn(s"Received BarEvent: $event").provide(environment)
           }
      _ <- publisher.publish(FooEvent())
      _ <- publisher.publish(BarEvent())
      _ <- publisher.run.use { pubSub =>
             for {
               _ <- console.putStrLn(s"Publishing in Use")
               _ <- pubSub.publish(FooEvent())
               _ <- pubSub.publish(BarEvent())
               _ <- pubSub.publish(FooEvent())
               _ <- console.putStrLn(s"Done Publishing in Use")
             } yield ()
           }
    } yield ()
  ).exitCode

  sealed trait MyEvent
  object MyEvent {
    final case class FooEvent() extends MyEvent
    final case class BarEvent() extends MyEvent
  }
}
