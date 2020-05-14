package morphir.io.file

import zio._

sealed abstract class VFile {
  def path: VFilePath
  def text: ZIO[Any, Throwable, Option[String]]
}

object VFile {

  def apply(path: VFilePath, contents: String): VFile =
    VFileInternal(path, VFileContents.Text(contents))

  def make(path: VFilePath, contents: String): UIO[VFile] = ZIO.succeed(VFile(path, contents))

  def makeM[S](path: UIO[VFilePath], contents: Task[Option[String]]): UIO[VFile] =
    path.map(p => VFileInternal(p, VFileContents.Delayed(contents)))

  def makeM(path: VFilePath, contents: Task[Option[String]]): UIO[VFile] =
    ZIO.succeed(VFileInternal(path, VFileContents.Delayed(contents)))

  private[file] final case class VFileInternal(path: VFilePath, contents: VFileContents) extends VFile {

    override def text: ZIO[Any, Throwable, Option[String]] = contents match {
      case VFileContents.Text(contents) => ZIO.succeed(Option(contents))
      case VFileContents.Delayed(get)   => get
    }
  }

  sealed abstract class VFileContents
  object VFileContents {
    final case class Text(contents: String)             extends VFileContents
    final case class Delayed(get: Task[Option[String]]) extends VFileContents

  }
}
