package morphir.flowz.eventing

import zio.Chunk
import zio.stream.ZSink
import zio.test._
import zio.test.Assertion._
object InMemoryEventLogSpec extends DefaultRunnableSpec {
  def spec = suite("InMemoryEventLog Spec")(
    suite("Construction")(
      testM("Its should support construction")(
        for {
          widgetEventLog <- eventLog.EventLog.Service.inMemory[WidgetEvent]
        } yield assert(widgetEventLog)(not(isNull))
      )
    ),
    suite("Persistence")(
      testM("It should support persisting events")(
        for {
          eventLog <- eventLog.EventLog.Service.inMemory[WidgetEvent]
          event1    = WidgetEvent.WidgetCreated("ACME Standard Widget")
          event2    = WidgetEvent.WidgetCreated("ACE Standard Widget")
          _        <- eventLog.persistEvent(EventStreamId("Widget Registry"), event1)
          _        <- eventLog.persistEvent(EventStreamId("Widget Registry"), event2)
          actual   <- eventLog.loadEvents(EventStreamId("Widget Registry")).run(ZSink.collectAll)
          other    <- eventLog.loadEvents(EventStreamId("Alt Widget Registry")).run(ZSink.collectAll)
        } yield assert(actual)(equalTo(Chunk(event1, event2))) && assert(other)(isEmpty)
      )
    )
  )

  sealed trait WidgetEvent
  object WidgetEvent {
    final case class WidgetCreated(name: String) extends WidgetEvent
  }

  sealed trait SprocketEvent
  object SprocketEvent {
    final case class SprocketCreated(name: String) extends SprocketEvent
  }
}
