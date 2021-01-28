package morphir.flowz.eventing

import zio._
import zio.stm._
import zio.stream._

trait EventChannel[Msg] {
  def publish(message: Msg): UIO[Unit]
  def subscribe: Stream[Nothing, Msg]
}

object EventChannel {
  def bounded[Msg](n: Int): UIO[EventChannel[Msg]] =
    (for {
      writeIndex         <- TRef.make(0)
      subscriberProgress <- TMap.make[Int, Int]()
      subscriberIdGen    <- TRef.make(0)
      buffer             <- TArray.make(List.fill[Option[Msg]](n)(None): _*)
    } yield new EventChannel[Msg] {
      def publish(message: Msg): UIO[Unit] =
        (for {
          writeIdx <- writeIndex.get
          slowestReaderIdx <- subscriberProgress.fold(-1) { case (idx, (_, subscriberIdx)) =>
                                if (idx < 0) subscriberIdx
                                else
                                  idx.min(subscriberIdx)
                              }
          _ <- STM.check((writeIdx - slowestReaderIdx) < n)
          _ <- buffer.update(writeIdx % n, _ => Some(message))
          _ <- writeIndex.update(_ + 1)
        } yield ()).commit

      def subscribe: Stream[Nothing, Msg] =
        Stream.unwrap(
          for {
            subscriberId <- subscriberIdGen.updateAndGet(_ + 1).commit // Generate a unique subscriber id
            writeIdx     <- writeIndex.get.tap(idx => subscriberProgress.put(subscriberId, idx)).commit
            readIndex    <- TRef.make(writeIdx).commit
          } yield Stream
            .repeatEffect((for {
              readIdx  <- readIndex.get
              writeIdx <- writeIndex.get
              _        <- STM.check(writeIdx - readIdx > 0) // Make sure something is available to read
              message  <- buffer(readIdx % n).map(_.get)
              _        <- readIndex.updateAndGet(_ + 1).flatMap(idx => subscriberProgress.put(subscriberId, idx))
            } yield message).commit)
            .ensuring(subscriberProgress.delete(subscriberId).commit)
        )

    }).commit
}
