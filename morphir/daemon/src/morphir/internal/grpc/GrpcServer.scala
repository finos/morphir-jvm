package morphir.internal.grpc

import io.grpc.{BindableService, Server, ServerBuilder, ServerServiceDefinition}
import zio._
import zio.clock

import com.google.api.Service
import io.grpc.ServerServiceDefinition
import java.time.OffsetDateTime

object GrpcServer {

  def make(
      port: Int,
      service: ServerServiceDefinition,
      otherServices: ServerServiceDefinition*
  ): UIO[GrpcServer] =
    for {
      server <- ZIO.effectTotal {
        val builder = ServerBuilder.forPort(port).addService(service)
        builder.build()
      }
      serverRef <- Ref.make(server)
    } yield new GrpcServer(serverRef)

  sealed trait State

  object State {
    case object Pending extends State
    case class Started(
        startTime: OffsetDateTime,
        private val shutdownHandle: Promise[Throwable, OffsetDateTime]
    ) extends State {
      def shutdown: ZIO[clock.Clock, Throwable, State] =
        for {
          shutdownRequestedAt <- clock.currentDateTime
          f <- shutdownHandle.await.fork
          _ <- shutdownHandle.succeed(shutdownRequestedAt)
          _ <- f.join
          shutdownTime <- clock.currentDateTime
        } yield Stopped(shutdownRequestedAt, shutdownTime)
    }

    case class ShutdownPending(shutdownRequestedAt: OffsetDateTime)
        extends State
    case class Stopped(
        shutdownRequestedAt: OffsetDateTime,
        shutdownTime: OffsetDateTime
    ) extends State
  }

}

class GrpcServer private (server: Ref[Server]) {
  def run: Task[GrpcServer] =
    ZIO.bracket(
      server.get,
      (svr: Server) => ZIO.effectTotal { svr.shutdownNow() },
      (svr: Server) => ZIO.effect(svr.start()) *> UIO.succeed(this)
    )

  def start: ZIO[clock.Clock, Throwable, GrpcServer.State] =
    for {
      promise <- Promise.make[Throwable, OffsetDateTime]
      svr <- server.get
      _ <- ZIO.effect { svr.start() }
      startTime <- clock.currentDateTime
    } yield GrpcServer.State.Started(startTime, promise)

}
