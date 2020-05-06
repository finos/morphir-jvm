package morphir.io.file

import cats.data.NonEmptyList
import cats.implicits._
import cats.{ Order, Show }
import io.circe.{ Decoder, Encoder }

import scala.annotation.tailrec

sealed trait FileSystemPath[+B, +T, +S] {
  def isAbsolute: Boolean
  def isRelative: Boolean = !isAbsolute
}

object FileSystemPath {
  sealed trait Relative
  sealed trait Absolute

  sealed trait File
  sealed trait Dir

  sealed trait Sandboxed
  sealed trait Unsandboxed

  final case class FileName(value: String) extends AnyVal {
    def extension: String = {
      val index = value.lastIndexOf('.')
      if (index == -1) "" else value.substring(index + 1)
    }

    def withoutExtension: String = {
      val index = value.lastIndexOf(".")
      if (index == -1) this.value else value.substring(0, index)
    }

    def FileNameWithoutExtension: FileName = {
      val index = value.lastIndexOf(".")
      if (index == -1) this else FileName(value.substring(0, index))
    }

    def changeExtension(f: String => String): FileName =
      FileName(withoutExtension + "." + f(extension))

    def changeExtension(newExtension: String): FileName =
      FileName(withoutExtension + "." + newExtension)
  }

  object FileName {

    implicit val encodeFileNme: Encoder[FileName] = Encoder.encodeString.contramap(fn => fn.value)

    implicit val decodeFileName: Decoder[FileName] = Decoder.decodeString.map(FileName(_))

    implicit val show: Show[FileName]   = Show.show(_.value)
    implicit val order: Order[FileName] = Order.by(_.value)
  }

  final case class DirName(value: String) extends AnyVal
  object DirName {

    implicit val encodeDirName: Encoder[DirName] = Encoder.encodeString.contramap(fn => fn.value)

    implicit val decodeDirName: Decoder[DirName] = Decoder.decodeString.map(DirName(_))

    implicit val show: Show[DirName]   = Show.show(_.value)
    implicit val order: Order[DirName] = Order.by(_.value)
  }

  private case object Current extends FileSystemPath[Nothing, Nothing, Nothing] {
    def isAbsolute = false
  }

  private case object Root extends FileSystemPath[Nothing, Nothing, Nothing] {
    def isAbsolute = true
  }

  private final case class ParentPath[B, T, S](parent: FileSystemPath[B, T, S]) extends FileSystemPath[B, T, S] {
    def isAbsolute: Boolean = parent.isAbsolute
  }

  private final case class DirPath[B, T, S](parent: FileSystemPath[B, T, S], name: DirName)
      extends FileSystemPath[B, T, S] {
    def isAbsolute: Boolean = parent.isAbsolute
  }

  private final case class FilePath[B, T, S](parent: FileSystemPath[B, T, S], name: FileName)
      extends FileSystemPath[B, T, S] {
    def isAbsolute: Boolean = parent.isAbsolute
  }

  type RelativeFile[S] = FileSystemPath[Relative, File, S]
  type AbsoluteFile[S] = FileSystemPath[Absolute, File, S]
  type RelativeDir[S]  = FileSystemPath[Relative, Dir, S]
  type AbsoluteDir[S]  = FileSystemPath[Absolute, Dir, S]

  def currentDir[S]: RelativeDir[S]             = Current
  def rootDir[S]: AbsoluteDir[S]                = Root
  def file[S](name: String): RelativeFile[S]    = file1[S](FileName(name))
  def file1[S](name: FileName): RelativeFile[S] = FilePath(Current, name)

  def fileName[B, S](path: FileSystemPath[B, File, S]): FileName = path match {
    case FilePath(_, name) => name
    case _                 => sys.error("impossible!")
  }

  def dir[S](name: String): FileSystemPath[Relative, Dir, S]   = dir1[S](DirName(name))
  def dir1[S](name: DirName): FileSystemPath[Relative, Dir, S] = DirPath(Current, name)
  def dirname[B, S](path: FileSystemPath[B, Dir, S]): Option[DirName] = path match {
    case DirPath(_, name) => Some(name)
    case _                => None
  }

  implicit class PathOps[B, T, S](path: FileSystemPath[B, T, S]) {
    def relativeTo[SS](dir: FileSystemPath[B, Dir, SS]): Option[FileSystemPath[Relative, T, SS]] = {
      @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
      def go[TT](
        p1: FileSystemPath[B, TT, S],
        p2: FileSystemPath[B, Dir, SS]
      ): Option[FileSystemPath[Relative, TT, SS]] =
        if (identicalPath(p1, p2)) Some(Current)
        else
          peel(p1) match {
            case None =>
              (p1, p2) match {
                case (Root, Root)       => Some(Current)
                case (Current, Current) => Some(Current)
                case _                  => None
              }
            case Some((p1p, v)) =>
              go(p1p, p2).map(p =>
                p </> v.fold[FileSystemPath[Relative, TT, SS]](DirPath(Current, _), FilePath(Current, _))
              )
          }
      go(canonicalize(path), canonicalize(dir))
    }
  }

  implicit class DirOps[B, S](dir: FileSystemPath[B, Dir, S]) {
    def </>[T](rel: FileSystemPath[Relative, T, S]): FileSystemPath[B, T, S] =
      (dir, rel) match {
        case (Current, Current)          => Current
        case (Root, Current)             => Root
        case (ParentPath(p1), Current)   => ParentPath(p1 </> Current)
        case (FilePath(p1, f1), Current) => FilePath(p1 </> Current, f1)
        case (DirPath(p1, d1), Current)  => DirPath(p1 </> Current, d1)

        // these don't make sense, but cannot exist anyway
        case (Current, Root)          => Current
        case (Root, Root)             => Root
        case (ParentPath(p1), Root)   => ParentPath(p1 </> Current)
        case (FilePath(p1, f1), Root) => FilePath(p1 </> Current, f1)
        case (DirPath(p1, d1), Root)  => DirPath(p1 </> Current, d1)

        case (p1, ParentPath(p2))   => ParentPath(p1 </> p2)
        case (p1, FilePath(p2, f2)) => FilePath(p1 </> p2, f2)
        case (p1, DirPath(p2, d2))  => DirPath(p1 </> p2, d2)
      }

    // NB: scala doesn't cotton to `<..>`
    def <::>[T](rel: FileSystemPath[Relative, T, S]): FileSystemPath[B, T, Unsandboxed] =
      parentDir1(dir) </> unsandbox(rel)

    @inline def up[T](rel: FileSystemPath[Relative, T, S]): FileSystemPath[B, T, Unsandboxed] =
      <::>(rel)
  }

  implicit class FileOps[B, S](file: FileSystemPath[B, File, S]) {
    // NB: scala doesn't cotton to `<.>`
    def <:>(ext: String): FileSystemPath[B, File, S] =
      renameFile(file, name => name.changeExtension(_ => ext))
  }

  def refineType[B, T, S](
    path: FileSystemPath[B, T, S]
  ): Either[FileSystemPath[B, Dir, S], FileSystemPath[B, File, S]] = path match {
    case Current        => Left(Current)
    case Root           => Left(Root)
    case ParentPath(p)  => Left(ParentPath(unsafeCoerceType(p)))
    case FilePath(p, f) => Right(FilePath(unsafeCoerceType(p), f))
    case DirPath(p, d)  => Left(DirPath(unsafeCoerceType(p), d))
  }

  def maybeDir[B, T, S](path: FileSystemPath[B, T, S]): Option[FileSystemPath[B, Dir, S]] =
    refineType(path).swap.toOption

  def maybeFile[B, T, S](path: FileSystemPath[B, T, S]): Option[FileSystemPath[B, File, S]] =
    refineType(path).toOption

  def peel[B, T, S](path: FileSystemPath[B, T, S]): Option[(FileSystemPath[B, Dir, S], Either[DirName, FileName])] =
    path match {
      case Current => None
      case Root    => None
      case p @ ParentPath(_) =>
        val (c, p1) = canonicalize1(p)
        if (c) peel(p1) else None
      case DirPath(p, d)  => Some(unsafeCoerceType(p) -> Left(d))
      case FilePath(p, f) => Some(unsafeCoerceType(p) -> Right(f))
    }

  def depth[B, T, S](path: FileSystemPath[B, T, S]): Int = path match {
    case Current        => 0
    case Root           => 0
    case ParentPath(p)  => depth(p) - 1
    case FilePath(p, _) => depth(p) + 1
    case DirPath(p, _)  => depth(p) + 1
  }

  def identicalPath[B, T, S, BB, TT, SS](p1: FileSystemPath[B, T, S], p2: FileSystemPath[BB, TT, SS]): Boolean =
    p1.show == p2.show

  def parentDir[B, T, S](path: FileSystemPath[B, T, S]): Option[FileSystemPath[B, Dir, S]] =
    peel(path).map(_._1)

  def fileParent[B, S](file: FileSystemPath[B, File, S]): FileSystemPath[B, Dir, S] = file match {
    case FilePath(p, _) => unsafeCoerceType(p)
    case _              => sys.error("impossible!")
  }

  def unsandbox[B, T, S](path: FileSystemPath[B, T, S]): FileSystemPath[B, T, Unsandboxed] = path match {
    case Current        => Current
    case Root           => Root
    case ParentPath(p)  => ParentPath(unsandbox(p))
    case DirPath(p, d)  => DirPath(unsandbox(p), d)
    case FilePath(p, f) => FilePath(unsandbox(p), f)
  }

  def sandbox[B, T, S](
    dir: FileSystemPath[B, Dir, Sandboxed],
    path: FileSystemPath[B, T, S]
  ): Option[FileSystemPath[Relative, T, Sandboxed]] =
    path relativeTo dir

  def parentDir1[B, T, S](path: FileSystemPath[B, T, S]): FileSystemPath[B, Dir, Unsandboxed] =
    ParentPath(unsafeCoerceType(unsandbox(path)))

  private def unsafeCoerceType[B, T, TT, S](path: FileSystemPath[B, T, S]): FileSystemPath[B, TT, S] = path match {
    case Current        => Current
    case Root           => Root
    case ParentPath(p)  => ParentPath(unsafeCoerceType(p))
    case DirPath(p, d)  => DirPath(unsafeCoerceType(p), d)
    case FilePath(p, f) => FilePath(unsafeCoerceType(p), f)
  }

  def renameFile[B, S](path: FileSystemPath[B, File, S], f: FileName => FileName): FileSystemPath[B, File, S] =
    path match {
      case FilePath(p, f0) => FilePath(p, f(f0))
      case p               => p
    }

  def renameDir[B, S](path: FileSystemPath[B, Dir, S], f: DirName => DirName): FileSystemPath[B, Dir, S] =
    path match {
      case DirPath(p, d) => DirPath(p, f(d))
      case p             => p
    }

  def canonicalize[B, T, S](path: FileSystemPath[B, T, S]): FileSystemPath[B, T, S] =
    canonicalize1(path)._2

  private def canonicalize1[B, T, S](path: FileSystemPath[B, T, S]): (Boolean, FileSystemPath[B, T, S]) =
    path match {
      case Current                    => false -> Current
      case Root                       => false -> Root
      case ParentPath(FilePath(p, _)) => true  -> canonicalize1(p)._2
      case ParentPath(DirPath(p, _))  => true  -> canonicalize1(p)._2
      case ParentPath(p) =>
        val (ch, p1) = canonicalize1(p)
        val p2       = ParentPath(p1)
        if (ch) canonicalize1(p2) else ch -> p2 // ???
      case FilePath(p, f) =>
        val (ch, p1) = canonicalize1(p)
        ch -> FilePath(p1, f)
      case DirPath(p, d) =>
        val (ch, p1) = canonicalize1(p)
        ch -> DirPath(p1, d)
    }

  def flatten[X](
    root: => X,
    currentDir: => X,
    parentDir: => X,
    dirName: String => X,
    fileName: String => X,
    path: FileSystemPath[_, _, _]
  ): NonEmptyList[X] = {
    @tailrec
    def go(xs: NonEmptyList[X], at: FileSystemPath[_, _, _]): NonEmptyList[X] = {
      val tl = xs.head :: xs.tail

      at match {
        case Current        => NonEmptyList(currentDir, tl)
        case Root           => NonEmptyList(root, tl)
        case ParentPath(p)  => go(NonEmptyList(parentDir, tl), p)
        case DirPath(p, d)  => go(NonEmptyList(dirName(d.value), tl), p)
        case FilePath(p, f) => go(NonEmptyList(fileName(f.value), tl), p)
      }
    }

    path match {
      case Current        => NonEmptyList(currentDir, List.empty)
      case Root           => NonEmptyList(root, List.empty)
      case ParentPath(p)  => go(NonEmptyList(parentDir, List.empty), p)
      case DirPath(p, d)  => go(NonEmptyList(dirName(d.value), List.empty), p)
      case FilePath(p, f) => go(NonEmptyList(fileName(f.value), List.empty), p)
    }
  }

  val posixCodec: PathCodec   = PathCodec placeholder '/'
  val windowsCodec: PathCodec = PathCodec placeholder '\\'

  final case class PathCodec(separator: Char, escape: String => String, unescape: String => String) {
    def unsafePrintPath(path: FileSystemPath[_, _, _]): String = {
      val s: String = flatten("", ".", "..", escape, escape, path)
        .intercalate(separator.toString)

      maybeDir(path) match {
        case Some(_) => show"$s$separator"
        case None    => s
      }
    }

    def printPath[B, T](path: FileSystemPath[B, T, Sandboxed]): String =
      unsafePrintPath(path)

    def parsePath[Z](
      rf: RelativeFile[Unsandboxed] => Z,
      af: AbsoluteFile[Unsandboxed] => Z,
      rd: RelativeDir[Unsandboxed] => Z,
      ad: AbsoluteDir[Unsandboxed] => Z
    )(str: String): Z = {

      val segs  = str.split(separator)
      val isAbs = str.startsWith(separator.toString)
      val isDir =
        List(separator.toString, s"$separator.", s"$separator..").exists(str.endsWith) || str === "." || str === ".."

      def folder[B, S](base: FileSystemPath[B, Dir, S], segments: String): FileSystemPath[B, Dir, S] = segments match {
        case ""   => base
        case "."  => base
        case ".." => ParentPath(base)
        case seg  => base </> dir(unescape(seg))
      }

      if (str === "")
        rd(Current)
      else if (isAbs && !isDir)
        af(
          segs.init
            .foldLeft[AbsoluteDir[Unsandboxed]](rootDir[Unsandboxed])(folder) </> file[Unsandboxed](unescape(segs.last))
        )
      else if (isAbs && isDir)
        ad(segs.foldLeft[AbsoluteDir[Unsandboxed]](rootDir[Unsandboxed])(folder))
      else if (!isAbs && !isDir)
        rf(segs.init.foldLeft[RelativeDir[Unsandboxed]](Current)(folder) </> file[Unsandboxed](unescape(segs.last)))
      else
        rd(segs.foldLeft[RelativeDir[Unsandboxed]](Current)(folder))

    }

    val parseRelativeFile: String => Option[RelativeFile[Unsandboxed]] =
      parsePath[Option[RelativeFile[Unsandboxed]]](Some(_), _ => None, _ => None, _ => None)

    val parseAbsoluteFile: String => Option[AbsoluteFile[Unsandboxed]] =
      parsePath[Option[AbsoluteFile[Unsandboxed]]](_ => None, Some(_), _ => None, _ => None)

    val parseRelativeDir: String => Option[RelativeDir[Unsandboxed]] =
      parsePath[Option[RelativeDir[Unsandboxed]]](_ => None, _ => None, Some(_), _ => None)

    val parseAbsoluteDir: String => Option[AbsoluteDir[Unsandboxed]] =
      parsePath[Option[AbsoluteDir[Unsandboxed]]](_ => None, _ => None, _ => None, Some(_))

    private def asDir[B, S](path: FileSystemPath[B, File, S]): FileSystemPath[B, Dir, S] = path match {
      case FilePath(p, FileName(n)) => DirPath(unsafeCoerceType(p), DirName(n))
      case _                        => sys.error("impossible!")
    }

    val parseRelativeAsDir: String => Option[RelativeDir[Unsandboxed]] =
      parsePath[Option[RelativeDir[Unsandboxed]]](p => Some(asDir(p)), _ => None, Some(_), _ => None)

    val parseAbsoluteAsDir: String => Option[AbsoluteDir[Unsandboxed]] =
      parsePath[Option[AbsoluteDir[Unsandboxed]]](_ => None, p => Some(asDir(p)), _ => None, Some(_))

    implicit def encodeFileSystemPath[B, T, S]: Encoder[FileSystemPath[B, T, S]] =
      Encoder.encodeString.contramap(p => unsafePrintPath(p))
  }

  object PathCodec {

    def placeholder(sep: Char): PathCodec = {
      val escapeSep   = (_: String).replaceAllLiterally(sep.toString, $sep$)
      val unescapeSep = (_: String).replaceAllLiterally($sep$, sep.toString)

      PathCodec(sep, escapeRel compose escapeSep, unescapeSep compose unescapeRel)
    }

    private val escapeRel = (s: String) => if (s === "..") $dotdot$ else if (s === ".") $dot$ else s

    private val unescapeRel = (s: String) => if (s === $dotdot$) ".." else if (s == $dot$) "." else s

    private val $sep$    = "$sep$"
    private val $dot$    = "$dot$"
    private val $dotdot$ = "$dotdot$"
  }

  implicit def pathShow[B, T, S]: Show[FileSystemPath[B, T, S]] = new Show[FileSystemPath[B, T, S]] {
    override def show(t: FileSystemPath[B, T, S]): String = t match {
      case Current                  => "currentDir"
      case Root                     => "rootDir"
      case ParentPath(p)            => show"parentDir($p)"
      case DirPath(p, DirName(d))   => show"$p </> dir($d)"
      case FilePath(p, FileName(f)) => show"$p </> file($f)"
    }
  }

}