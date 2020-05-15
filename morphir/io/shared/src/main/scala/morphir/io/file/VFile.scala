package morphir.io.file

import java.io.FileNotFoundException

import zio._
import zio.stream._

import scala.io._

sealed abstract class VFile {
  def path: VFilePath
  def contents: ZIO[Any, Throwable, Option[String]]
  def children: ZStream[Any, Throwable, VFile]

  def split(f: VFile => Chunk[VFile]): Stream[Nothing, VFile] =
    ZStream.fromChunk(f(this))

  def text: ZIO[Any, Throwable, Option[String]] = contents
}

object VFile {

  def apply(path: VFilePath, contents: String): VFile =
    new VFileItem(path, contents)

  def make(path: VFilePath, contents: String): UIO[VFile] = ZIO.succeed(VFile(path, contents))

  def makeM[S](path: UIO[VFilePath], contents: Task[Option[String]]): UIO[VFile] =
    path.map(p => VFileItem(p, contents))

  def makeM(path: VFilePath, contents: Task[Option[String]]): UIO[VFile] =
    ZIO.succeed(VFileItem(path, contents))

  def fromResource(resourcePath: String, cls: Class[_] = getClass)(
    implicit codec: Codec
  ): ZManaged[Any, Throwable, VFile] = {
    val getResource: ZIO[Any, Throwable, BufferedSource] =
      ZIO
        .effect(Option(cls.getResourceAsStream(resourcePath)))
        .someOrFail(new FileNotFoundException(resourcePath))
        .map(stream => Source.fromInputStream(stream))

    ZManaged.make(getResource)(s => ZIO.effect(s.close()).orDie).mapM { (src: BufferedSource) =>
      val path     = VFilePath.forResource(resourcePath)
      val contents = ZIO.effect(src.mkString).map(Option(_))
      VFile.makeM(path, contents)
    }
  }

  private[VFile] final case class VFileItem(path: VFilePath, contents: ZIO[Any, Throwable, Option[String]])
      extends VFile {
    def this(path: VFilePath, contents: String) =
      this(path, ZIO.succeed(Option(contents)))

    def children: ZStream[Any, Throwable, VFile] = ZStream.empty
  }

  private[VFile] final case class VFolder(path: VFilePath, children: ZStream[Any, Throwable, VFile]) extends VFile {
    def contents: ZIO[Any, Throwable, Option[String]] = Contents.empty
  }

  object Contents {
    val empty: ZIO[Any, Throwable, Option[String]] = ZIO.none
  }
}
