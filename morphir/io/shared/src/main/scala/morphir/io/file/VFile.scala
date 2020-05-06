package morphir.io.file

import java.nio.charset.{ Charset, StandardCharsets }

import zio._

sealed abstract class VFile[+T] {
  def path: UnrestrictedFsPath[T]
  def text(encoding: Charset = StandardCharsets.UTF_8): ZIO[Any, Throwable, Option[String]]
}

object VFile {

  def apply[S](path: FilePath[S], contents: String): VFile[S] =
    VFileInternal(path, VFileContents.Text(contents))

  def make[S](path: FilePath[S], contents: String): UIO[VFile[S]] = ZIO.succeed(VFile(path, contents))
  def makeM[S](path: UIO[FilePath[S]], contents: Task[Option[String]]): UIO[VFile[S]] =
    path.map(p => VFileInternal(p, VFileContents.Delayed(contents)))
  def makeM[S](path: FilePath[S], contents: Task[Option[String]]): UIO[VFile[S]] =
    ZIO.succeed(VFileInternal(path, VFileContents.Delayed(contents)))

  private[file] final case class VFileInternal[S](path: UnrestrictedFsPath[S], contents: VFileContents)
      extends VFile[S] {

    override def text(encoding: Charset): ZIO[Any, Throwable, Option[String]] = contents match {
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
