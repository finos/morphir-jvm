package morphir.daemon

import scala.concurrent.{ExecutionContext, Future}
import io.grpc.{Server, ServerBuilder}
import morphir.gateway._
import morphir.gateway.ZioGateway.Gateway
import morphir.runtime._
import java.time.OffsetDateTime
import zio._

object gateway {
  type GatewayService = Has[ZioGateway.Gateway]

  def live: ZLayer.NoDeps[Nothing, GatewayService] = ZLayer.fromFunction {
    _: Any =>
      new Gateway {
        def about(
            request: morphir.gateway.AboutRequest
        ): _root_.zio.IO[io.grpc.Status, morphir.gateway.AboutResponse] =
          IO.succeed(AboutResponse(daemonVersion = "999.99.99"))

        def initialize(
            request: morphir.gateway.InitializeRequest
        ): _root_.zio.IO[io.grpc.Status, morphir.gateway.InitializeResponse] =
          IO.succeed(InitializeResponse(port = 42))

      }
  }
}
