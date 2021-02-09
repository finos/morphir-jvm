package morphir.flowz

import zio._
import zio.clock.Clock
import zio.stm.{ TMap, TRef, ZSTM }

import java.time.Instant

object uidGenerator {
  type UidGenerator = Has[UidGenerator.Service]

  final case class Uid(namespace: String, timeStamp: Instant, seqNumber: Int) {
    override def toString: String = s"$namespace:${timeStamp.toString}:${"%07d".format(seqNumber)}"
  }

  def nextUid(namespace: String): URIO[UidGenerator, Uid] =
    ZIO.accessM(_.get.nextUid(namespace))

  def nextUid[Type](implicit tag: Tag[Type]): URIO[UidGenerator, Uid] =
    ZIO.accessM(_.get.nextUid)

  object UidGenerator {
    trait Service {
      def nextUid(namespace: String): UIO[Uid]
      final def nextUid[Type](implicit tag: Tag[Type]): UIO[Uid] = nextUid(tag.tag.shortName)
    }
    object Service {
      private[uidGenerator] def live(
        clock: Clock.Service,
        instantRef: TRef[Instant],
        counterMap: TMap[String, Int]
      ): Service =
        Live(clock, instantRef, counterMap)
      private final case class Live(clock: Clock.Service, instantRef: TRef[Instant], counterMap: TMap[String, Int])
          extends Service {
        def nextUid(namespace: String): UIO[Uid] =
          for {
            currentTs <- clock.instant
            uid       <- getNextUid(namespace, currentTs).commit
          } yield uid

        private def getNextUid(namespace: String, currentTimestamp: Instant) =
          for {
            lastTs <- instantRef.get
            _ <-
              if (currentTimestamp.isAfter(lastTs)) instantRef.set(currentTimestamp) *> counterMap.put(namespace, 0)
              else ZSTM.unit
            seqNo <- counterMap.getOrElse(namespace, 0).map(_ + 1)
            _     <- counterMap.put(namespace, seqNo)
          } yield Uid(namespace, currentTimestamp, seqNo)
      }
    }

    val live: ZLayer[Clock, Nothing, UidGenerator] = ZLayer.fromServiceM { clock: Clock.Service =>
      for {
        now        <- clock.instant
        instantRef <- TRef.makeCommit(now)
        counterMap <- TMap.empty[String, Int].commit
      } yield Service.live(clock, instantRef, counterMap)
    }
  }
}

object UidGenDemo extends App {
  import uidGenerator.Uid
  final case class Foo(id: Uid, idx: Int = 0)
  final case class Bar(id: Uid, idx: Int = 0)

  def run(args: List[String]): URIO[ZEnv, ExitCode] =
    (for {
      foos <- ZIO.collectAll(
                NonEmptyChunk
                  .fromIterable(0, 1 to 1000)
                  .map((idx: Int) => uidGenerator.nextUid[Foo].map(uid => Foo(uid, idx)))
              )
      foo1 <- uidGenerator.nextUid[Foo].map(Foo(_))
      foo2 <- uidGenerator.nextUid[Foo].map(Foo(_))
      bar1 <- uidGenerator.nextUid[Bar].map(Bar(_))
      bar2 <- uidGenerator.nextUid[Bar].map(Bar(_))
      _    <- console.putStrLn(s"Foo[1]: $foo1")
      _    <- console.putStrLn(s"Foo[2]: $foo2")
      _    <- console.putStrLn(s"Bar[1]: $bar1")
      _    <- console.putStrLn(s"Bar[2]: $bar2")
      _    <- ZIO.foreach(foos)(foo => console.putStrLn(s"Foo: $foo"))
    } yield ExitCode.success).provideCustomLayer(uidGenerator.UidGenerator.live)
}
