package morphir.flowz.eventing

import zio.{ Chunk, ZIO }
import zio.stream.ZSink
import zio.test._
import zio.test.Assertion._

object EventLogSpec extends DefaultRunnableSpec {
  def spec = suite("EventLog Spec")(
    testM("It should be possible to persist events")(
      for {
        records <- ZIO.succeed(List((1, "one"), (2, "two"), (3, "three")))
        _ <- eventLog
               .persistEvent[DataEvent](EventStreamId("test"), DataEvent.CreateTable(records))
        _ <- eventLog
               .persistEvent[DataEvent](EventStreamId("test"), DataEvent.ExcludeRecords(records.filter(_._1 < 2)))
        actual <- eventLog.loadEvents[DataEvent](EventStreamId("test")).run(ZSink.collectAll)
      } yield assert(actual)(
        equalTo(Chunk(DataEvent.CreateTable(records), DataEvent.ExcludeRecords(records.filter(_._1 < 2))))
      )
    ).provideCustomLayer(eventLog.EventLog.inMemory[DataEvent])
  )

  sealed trait DataEvent
  object DataEvent {
    final case class CreateTable[Record](records: List[Record])    extends DataEvent
    final case class ExcludeRecords[Record](records: List[Record]) extends DataEvent
  }
}
