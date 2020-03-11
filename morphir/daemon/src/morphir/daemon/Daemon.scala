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

object Daemon extends App {

  def serverWait: ZIO[Console with Clock, Throwable, Unit] =
    for {
      _ <- putStrLn("Server is running. Press Ctrl-C to stop.")
      _ <- (putStr(".") *> ZIO.sleep(1.second)).forever
    } yield ()

  def serverLive(port: Int): ZLayer.NoDeps[Nothing, Server] =
    Clock.live >>> gateway.live >>> Server.live[Gateway](
      ServerBuilder.forPort(port)
    )

  def run(args: List[String]) = myAppLogic.fold(_ => 1, _ => 0)

  val myAppLogic =
    serverWait.provideLayer(serverLive(8080) ++ Console.live ++ Clock.live)
}
