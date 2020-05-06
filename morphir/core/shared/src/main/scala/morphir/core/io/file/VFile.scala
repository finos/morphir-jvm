package morphir.core.io.file

import java.nio.charset.{ Charset, StandardCharsets }

import zio._

sealed abstract class VFile[+S] {
  def path: UnrestrictedFsPath[S]
  def text(encoding: Charset = StandardCharsets.UTF_8): ZIO[Any, Throwable, Option[String]]
}

object VFile {

  def apply[S](path: FilePath[S], contents: String): VFile[S] =
    VFileInternal(path, VFileContents.Text(contents))

  def make[S](path: FilePath[S], contents: String): UIO[VFile[S]] = ZIO.succeed(VFile(path, contents))
  def makeM[S](path: UIO[FilePath[S]], contents: Task[Option[String]]): UIO[VFile[S]] =
    path.map(p => VFileInternal(p, VFileContents.TextEffectual(contents)))
  def makeM[S](path: FilePath[S], contents: Task[Option[String]]): UIO[VFile[S]] =
    ZIO.succeed(VFileInternal(path, VFileContents.TextEffectual(contents)))

  private[file] final case class VFileInternal[S](path: UnrestrictedFsPath[S], contents: VFileContents)
      extends VFile[S] {

    override def text(encoding: Charset): ZIO[Any, Throwable, Option[String]] = contents match {
      case VFileContents.Text(contents)         => ZIO.succeed(Option(contents))
      case VFileContents.TextEffectual(get)     => get
      case VFileContents.Buffered(contents @ _) => ???
      case VFileContents.Unbound                => ???
      case VFileContents.Null                   => ???
    }
  }

  sealed abstract class VFileContents
  object VFileContents {
    final case class Text(contents: String)                   extends VFileContents
    final case class TextEffectual(get: Task[Option[String]]) extends VFileContents
    final case class Buffered(contents: Chunk[Byte])          extends VFileContents
    final case object Unbound                                 extends VFileContents
    final case object Null                                    extends VFileContents

  }
}
