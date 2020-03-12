package morphir.internal.grpc

import io.grpc.Status
import zio.{clock, console, IO, ZIO, Task}
import zio.console.Console
import zio.clock.Clock
import zio.duration._
import zio.stream.Stream
import zio.ZLayer
import zio.Managed
import zio.Has
import zio.Tagged
import zio.ZManaged

import io.grpc.ServerBuilder
import io.grpc.ManagedChannel
import io.grpc.ManagedChannelBuilder
import scalapb.zio_grpc.ZBindableService
import scala.collection.immutable.Stream.Cons

object GrpcServer {
  trait Service {
    def port: Task[Int]

    def shutdown: Task[Unit]

    def shutdownNow: Task[Unit]

    def start: Task[Unit]
  }

  private[grpc] class ServiceImpl(underlying: io.grpc.Server) extends Service {
    def port: Task[Int] = ZIO.effect(underlying.getPort())

    def shutdown: Task[Unit] = ZIO.effect(underlying.shutdown()).unit

    def start: Task[Unit] = ZIO.effect(underlying.start()).unit

    def shutdownNow: Task[Unit] = ZIO.effect(underlying.shutdownNow()).unit
  }

  def managed[S0](builder: => ServerBuilder[_], service0: S0)(
      implicit b0: ZBindableService[S0]
  ): ZManaged[Console with Clock, Throwable, Service] =
    (for {
      ssd0 <- b0.bindService(service0)
      ourBuilder = builder.addService(ssd0)
      server = new ServiceImpl(ourBuilder.build())
      _ <- server.start
      _ <- console.putStrLn(s"GRPC server started...")
    } yield server).toManaged(
      _.shutdownNow
        .flatMap(_ =>
          console.putStrLn("Shut down happened") *> ZIO.sleep(3.seconds)
        )
        .ignore
    )

  def live[S0: Tagged](
      builder: => ServerBuilder[_]
  )(
      implicit b0: ZBindableService[S0]
  ): ZLayer[Console with Clock with Has[S0], Nothing, GrpcServer] =
    ZLayer.fromServiceManaged { s0: S0 =>
      GrpcServer.managed(builder, s0).orDie
    } ++ Console.live ++ Clock.live

  def fromManaged(zm: Managed[Throwable, io.grpc.Server]) =
    ZLayer.fromManaged(zm.map(s => Has(new ServiceImpl(s))))
}
