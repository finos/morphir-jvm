package morphir.daemon
import morphir.gateway.GatewayGrpc.Gateway
import morphir.gateway.{AboutRequest, AboutResponse}
import scala.concurrent.Future
import morphir.gateway.{InitializeRequest, InitializeResponse}
import scala.concurrent.{ExecutionContext, Future}
import io.grpc.{Server, ServerBuilder}
import morphir.internal.grpc.GrpcServer
import morphir.gateway.GatewayGrpc
import zio._
import zio.clock._
import io.grpc.ServerServiceDefinition
import java.time.OffsetDateTime

class Daemon private (port: Int, executionContext: ExecutionContext) {
  def run: ZIO[Clock, Throwable, OffsetDateTime] =
    for {
      startTime <- clock.currentDateTime
      gateway <- ZIO.effect(
        Daemon.gatewayServiceDefinition(startTime, executionContext)
      )
      server <- GrpcServer.make(port, gateway)

      _ <- server.run

    } yield startTime
}

object Daemon {

  def make(port: Int, executionContext: ExecutionContext): UIO[Daemon] =
    UIO.effectTotal(new Daemon(port, executionContext))

  def gatewayServiceDefinition(
      startTime: OffsetDateTime,
      executionContext: ExecutionContext
  ): ServerServiceDefinition =
    GatewayGrpc.bindService(
      new GatewayService(startTime, executionContext),
      executionContext
    )
}
