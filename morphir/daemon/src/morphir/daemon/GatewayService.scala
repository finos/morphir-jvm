package morphir.daemon

import scala.concurrent.{ExecutionContext, Future}
import io.grpc.{Server, ServerBuilder}
import morphir.gateway._
import morphir.gateway.GatewayGrpc.Gateway
import morphir.runtime._
import java.time.OffsetDateTime
import zio._
class GatewayService(
    startTime: OffsetDateTime,
    executionContext: ExecutionContext
) extends Gateway {

  override def about(request: AboutRequest): Future[AboutResponse] = ???

  override def initialize(
      request: InitializeRequest
  ): Future[InitializeResponse] = ???

}

trait ZGateway {
  def about(request: AboutRequest): Task[AboutRequest] = ???
}
