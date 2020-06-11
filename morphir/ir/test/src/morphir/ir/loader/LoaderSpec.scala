package morphir.ir.loader

import zio._
import zio.test._
import zio.test.Assertion._
import zio.test.TestAspect._
import scala.io.Source
import java.io.IOException
import scala.io.BufferedSource

object LoaderSpec extends DefaultRunnableSpec {
  def spec =
    suite("Loader Spec")(
      suite("Loading from JSON String")(
        testM("Loading a simple package") {
          for {
            json   <- getManagedResource("morphir/ir/hello.ir.json").map(bs => bs.mkString).useNow
            pkgDef <- loadPackage(json).provideCustomLayer(Loader.live)
            _      <- console.putStrLn(s"Package: $pkgDef")
          } yield assert(pkgDef.modules)(isNonEmpty)
        }
      )
    ) @@ silent

  def getManagedResource(resource: String): Managed[IOException, BufferedSource] =
    Managed.make(IO.effect(Source.fromResource(resource)).refineToOrDie[IOException])(bs =>
      ZIO.effect(bs.close()).orDie
    )
}
