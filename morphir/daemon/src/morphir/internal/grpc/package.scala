package morphir.internal
import zio.Has
package object grpc {
  type GrpcServer = Has[GrpcServer.Service]
}
