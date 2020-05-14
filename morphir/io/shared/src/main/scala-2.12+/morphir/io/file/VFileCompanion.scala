package morphir.io.file

import zio._

import scala.io.{ BufferedSource, Source }

private[file] trait VFileCompanion {
  def fromResource(resource: String): ZManaged[Any, Throwable, VFile] =
    ZManaged.makeEffect(Source.fromResource(resource))(s => s.close()).mapM { (src: BufferedSource) =>
      val path     = VFilePath.forResource(resource)
      val contents = ZIO.effect(src.mkString).map(Option(_))
      VFile.makeM(path, contents)
    }
}
