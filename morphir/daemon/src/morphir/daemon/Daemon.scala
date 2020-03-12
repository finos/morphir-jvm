package morphir.daemon
import morphir.gateway.{AboutRequest, AboutResponse}
import scala.concurrent.Future
import morphir.gateway.{InitializeRequest, InitializeResponse}
import morphir.gateway.GatewayGrpc
import zio._
import zio.clock.Clock
import zio.console._
import zio.duration._
import io.grpc.{ServerBuilder, ServerServiceDefinition}
import java.time.OffsetDateTime
import scalapb.zio_grpc.Server
import morphir.gateway.ZioGateway.Gateway
import scalapb.zio_grpc.ZBindableService
import morphir.internal.grpc.GrpcServer

object Daemon extends App {

  def serverWait(port: Int): ZIO[Console with Clock, Throwable, Unit] =
    for {
      _ <- putStrLn(
        s"Server is running @ localhost:${port}. Press Ctrl-C to stop."
      )
      _ <- (putStr(
        s"Server is running @ localhost:${port}. Press Ctrl-C to stop."
      ) *> ZIO.sleep(60.second)).forever
    } yield ()

  def serverLive(port: Int): ZLayer[Console, Nothing, GrpcServer] =
    (gateway.live ++ Console.live ++ Clock.live) >>> GrpcServer.live[Gateway](
      ServerBuilder.forPort(port)
    )

  def run(args: List[String]) = live(8080).fold(_ => 1, _ => 0)

  def live(port: Int) =
    serverWait(port).provideLayer(
      serverLive(port) ++ Console.live ++ Clock.live
    )
}
