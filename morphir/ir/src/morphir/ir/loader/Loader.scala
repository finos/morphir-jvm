package morphir.ir.loader

import morphir.ir._
import zio._
import upickle.default._
import zio.console.Console

object Loader {
  trait Service {
    def loadPackage(
      source: String
    )(implicit packageDefReader: Reader[PackageDefinition]): IO[Throwable, PackageDefinition]
  }

  val live: ZLayer[Console, Nothing, Has[Service]] = ZLayer.fromService { (console: Console.Service) =>
    new Service {
      def loadPackage(
        source: String
      )(implicit packageDefReader: Reader[PackageDefinition]): IO[Throwable, PackageDefinition] =
        for {
          _      <- console.putStrLn(s"Source: $source")
          pkgDef <- IO.effect(read[PackageDefinition](source))
          _      <- console.putStrLn(s"Pkg Def: $pkgDef")
        } yield pkgDef
    }
  }

}
