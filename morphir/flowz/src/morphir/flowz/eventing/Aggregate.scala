package morphir.flowz.eventing
import zio._

class Aggregate[-E, +S] private[eventing] (
  streamId: EventStreamId,
  stateRef: Ref[S],
  aggregations: (S, E) => Task[S],
  persist: (EventStreamId, E) => Task[Unit]
) {
  val state: UIO[S] = stateRef.get

  def appendAll(evt: Iterable[E]): Task[Aggregate[E, S]] =
    ZIO.foreach(evt)(append).map(_.head)

  def append(evt: E): Task[Aggregate[E, S]] =
    for {
      _ <- persist(streamId, evt)
      _ <- appendNoPersist(evt)
    } yield this

  private[eventing] def appendNoPersist(evt: E): Task[Aggregate[E, S]] =
    for {
      curState <- state
      modified <- aggregations(curState, evt)
      _        <- stateRef.set(modified)
    } yield this

}
