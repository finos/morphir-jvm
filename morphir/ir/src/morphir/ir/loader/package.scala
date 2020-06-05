package morphir.ir

import zio._
import upickle.default._

package object loader {
  type Loader = Has[Loader.Service]

  def loadPackage(
    source: String
  )(implicit packageDefReader: Reader[PackageDefinition]): ZIO[Loader, Throwable, PackageDefinition] =
    ZIO.accessM(_.get.loadPackage(source))
}
