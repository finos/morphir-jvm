package morphir.ir.loader

import morphir.ir._
import zio._
import upickle.default._

object Loader {
  trait Service {
    def loadPackage(
      source: String
    )(implicit packageDefReader: Reader[PackageDefinition]): IO[Throwable, PackageDefinition]
  }

  object Service {

    val live: Service = new Service {
      def loadPackage(
        source: String
      )(implicit packageDefReader: Reader[PackageDefinition]): IO[Throwable, PackageDefinition] =
        IO.effect(read[PackageDefinition](source))
    }
  }

  val live: Layer[Nothing, Has[Service]] = ZLayer.succeed(Service.live)

}
