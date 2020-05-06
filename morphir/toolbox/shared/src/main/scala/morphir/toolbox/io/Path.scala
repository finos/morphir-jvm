package morphir.toolbox.io

import cats.data.NonEmptyList
import cats.implicits._
import cats.{ Order, Show }
import io.circe.{ Decoder, Encoder }

import scala.annotation.tailrec

sealed trait Path[+B, +T, +S] {
  def isAbsolute: Boolean
  def isRelative: Boolean = !isAbsolute
}

object Path {
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

  private case object Current extends Path[Nothing, Nothing, Nothing] {
    def isAbsolute = false
  }

  private case object Root extends Path[Nothing, Nothing, Nothing] {
    def isAbsolute = true
  }

  private final case class InternalParent[B, T, S](parent: Path[B, T, S]) extends Path[B, T, S] {
    def isAbsolute: Boolean = parent.isAbsolute
  }

  private final case class InternalDir[B, T, S](parent: Path[B, T, S], name: DirName) extends Path[B, T, S] {
    def isAbsolute: Boolean = parent.isAbsolute
  }

  private final case class InternalFile[B, T, S](parent: Path[B, T, S], name: FileName) extends Path[B, T, S] {
    def isAbsolute: Boolean = parent.isAbsolute
  }

  type RelativeFile[S] = Path[Relative, File, S]
  type AbsoluteFile[S] = Path[Absolute, File, S]
  type RelativeDir[S]  = Path[Relative, Dir, S]
  type AbsoluteDir[S]  = Path[Absolute, Dir, S]

  def currentDir[S]: RelativeDir[S]             = Current
  def rootDir[S]: AbsoluteDir[S]                = Root
  def file[S](name: String): RelativeFile[S]    = file1[S](FileName(name))
  def file1[S](name: FileName): RelativeFile[S] = InternalFile(Current, name)

  def fileName[B, S](path: Path[B, File, S]): FileName = path match {
    case InternalFile(_, name) => name
    case _                     => sys.error("impossible!")
  }

  def dir[S](name: String): Path[Relative, Dir, S]   = dir1[S](DirName(name))
  def dir1[S](name: DirName): Path[Relative, Dir, S] = InternalDir(Current, name)
  def dirname[B, S](path: Path[B, Dir, S]): Option[DirName] = path match {
    case InternalDir(_, name) => Some(name)
    case _                    => None
  }

  implicit class PathOps[B, T, S](path: Path[B, T, S]) {
    def relativeTo[SS](dir: Path[B, Dir, SS]): Option[Path[Relative, T, SS]] = {
      @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
      def go[TT](p1: Path[B, TT, S], p2: Path[B, Dir, SS]): Option[Path[Relative, TT, SS]] =
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
                p </> v.fold[Path[Relative, TT, SS]](InternalDir(Current, _), InternalFile(Current, _))
              )
          }
      go(canonicalize(path), canonicalize(dir))
    }
  }

  implicit class DirOps[B, S](dir: Path[B, Dir, S]) {
    def </>[T](rel: Path[Relative, T, S]): Path[B, T, S] =
      (dir, rel) match {
        case (Current, Current)              => Current
        case (Root, Current)                 => Root
        case (InternalParent(p1), Current)   => InternalParent(p1 </> Current)
        case (InternalFile(p1, f1), Current) => InternalFile(p1 </> Current, f1)
        case (InternalDir(p1, d1), Current)  => InternalDir(p1 </> Current, d1)

        // these don't make sense, but cannot exist anyway
        case (Current, Root)              => Current
        case (Root, Root)                 => Root
        case (InternalParent(p1), Root)   => InternalParent(p1 </> Current)
        case (InternalFile(p1, f1), Root) => InternalFile(p1 </> Current, f1)
        case (InternalDir(p1, d1), Root)  => InternalDir(p1 </> Current, d1)

        case (p1, InternalParent(p2))   => InternalParent(p1 </> p2)
        case (p1, InternalFile(p2, f2)) => InternalFile(p1 </> p2, f2)
        case (p1, InternalDir(p2, d2))  => InternalDir(p1 </> p2, d2)
      }

    // NB: scala doesn't cotton to `<..>`
    def <::>[T](rel: Path[Relative, T, S]): Path[B, T, Unsandboxed] =
      parentDir1(dir) </> unsandbox(rel)

    @inline def up[T](rel: Path[Relative, T, S]): Path[B, T, Unsandboxed] =
      <::>(rel)
  }

  implicit class FileOps[B, S](file: Path[B, File, S]) {
    // NB: scala doesn't cotton to `<.>`
    def <:>(ext: String): Path[B, File, S] =
      renameFile(file, name => name.changeExtension(_ => ext))
  }

  def refineType[B, T, S](path: Path[B, T, S]): Either[Path[B, Dir, S], Path[B, File, S]] = path match {
    case Current            => Left(Current)
    case Root               => Left(Root)
    case InternalParent(p)  => Left(InternalParent(unsafeCoerceType(p)))
    case InternalFile(p, f) => Right(InternalFile(unsafeCoerceType(p), f))
    case InternalDir(p, d)  => Left(InternalDir(unsafeCoerceType(p), d))
  }

  def maybeDir[B, T, S](path: Path[B, T, S]): Option[Path[B, Dir, S]] =
    refineType(path).swap.toOption

  def maybeFile[B, T, S](path: Path[B, T, S]): Option[Path[B, File, S]] =
    refineType(path).toOption

  @scala.annotation.tailrec
  def peel[B, T, S](path: Path[B, T, S]): Option[(Path[B, Dir, S], Either[DirName, FileName])] = path match {
    case Current => None
    case Root    => None
    case p @ InternalParent(_) =>
      val (c, p1) = canonicalize1(p)
      if (c) peel(p1) else None
    case InternalDir(p, d)  => Some(unsafeCoerceType(p) -> Left(d))
    case InternalFile(p, f) => Some(unsafeCoerceType(p) -> Right(f))
  }

  def depth[B, T, S](path: Path[B, T, S]): Int = path match {
    case Current            => 0
    case Root               => 0
    case InternalParent(p)  => depth(p) - 1
    case InternalFile(p, _) => depth(p) + 1
    case InternalDir(p, _)  => depth(p) + 1
  }

  def identicalPath[B, T, S, BB, TT, SS](p1: Path[B, T, S], p2: Path[BB, TT, SS]): Boolean =
    p1.show == p2.show

  def parentDir[B, T, S](path: Path[B, T, S]): Option[Path[B, Dir, S]] =
    peel(path).map(_._1)

  def fileParent[B, S](file: Path[B, File, S]): Path[B, Dir, S] = file match {
    case InternalFile(p, _) => unsafeCoerceType(p)
    case _                  => sys.error("impossible!")
  }

  def unsandbox[B, T, S](path: Path[B, T, S]): Path[B, T, Unsandboxed] = path match {
    case Current            => Current
    case Root               => Root
    case InternalParent(p)  => InternalParent(unsandbox(p))
    case InternalDir(p, d)  => InternalDir(unsandbox(p), d)
    case InternalFile(p, f) => InternalFile(unsandbox(p), f)
  }

  def sandbox[B, T, S](dir: Path[B, Dir, Sandboxed], path: Path[B, T, S]): Option[Path[Relative, T, Sandboxed]] =
    path relativeTo dir

  def parentDir1[B, T, S](path: Path[B, T, S]): Path[B, Dir, Unsandboxed] =
    InternalParent(unsafeCoerceType(unsandbox(path)))

  private def unsafeCoerceType[B, T, TT, S](path: Path[B, T, S]): Path[B, TT, S] = path match {
    case Current            => Current
    case Root               => Root
    case InternalParent(p)  => InternalParent(unsafeCoerceType(p))
    case InternalDir(p, d)  => InternalDir(unsafeCoerceType(p), d)
    case InternalFile(p, f) => InternalFile(unsafeCoerceType(p), f)
  }

  def renameFile[B, S](path: Path[B, File, S], f: FileName => FileName): Path[B, File, S] =
    path match {
      case InternalFile(p, f0) => InternalFile(p, f(f0))
      case p                   => p
    }

  def renameDir[B, S](path: Path[B, Dir, S], f: DirName => DirName): Path[B, Dir, S] =
    path match {
      case InternalDir(p, d) => InternalDir(p, f(d))
      case p                 => p
    }

  def canonicalize[B, T, S](path: Path[B, T, S]): Path[B, T, S] =
    canonicalize1(path)._2

  private def canonicalize1[B, T, S](path: Path[B, T, S]): (Boolean, Path[B, T, S]) =
    path match {
      case Current                            => false -> Current
      case Root                               => false -> Root
      case InternalParent(InternalFile(p, _)) => true  -> canonicalize1(p)._2
      case InternalParent(InternalDir(p, _))  => true  -> canonicalize1(p)._2
      case InternalParent(p) =>
        val (ch, p1) = canonicalize1(p)
        val p2       = InternalParent(p1)
        if (ch) canonicalize1(p2) else ch -> p2 // ???
      case InternalFile(p, f) =>
        val (ch, p1) = canonicalize1(p)
        ch -> InternalFile(p1, f)
      case InternalDir(p, d) =>
        val (ch, p1) = canonicalize1(p)
        ch -> InternalDir(p1, d)
    }

  def flatten[X](
    root: => X,
    currentDir: => X,
    parentDir: => X,
    dirName: String => X,
    fileName: String => X,
    path: Path[_, _, _]
  ): NonEmptyList[X] = {
    @tailrec
    def go(xs: NonEmptyList[X], at: Path[_, _, _]): NonEmptyList[X] = {
      val tl = xs.head :: xs.tail

      at match {
        case Current            => NonEmptyList(currentDir, tl)
        case Root               => NonEmptyList(root, tl)
        case InternalParent(p)  => go(NonEmptyList(parentDir, tl), p)
        case InternalDir(p, d)  => go(NonEmptyList(dirName(d.value), tl), p)
        case InternalFile(p, f) => go(NonEmptyList(fileName(f.value), tl), p)
      }
    }

    path match {
      case Current            => NonEmptyList(currentDir, List.empty)
      case Root               => NonEmptyList(root, List.empty)
      case InternalParent(p)  => go(NonEmptyList(parentDir, List.empty), p)
      case InternalDir(p, d)  => go(NonEmptyList(dirName(d.value), List.empty), p)
      case InternalFile(p, f) => go(NonEmptyList(fileName(f.value), List.empty), p)
    }
  }

  val posixCodec: PathCodec   = PathCodec placeholder '/'
  val windowsCodec: PathCodec = PathCodec placeholder '\\'

  final case class PathCodec(separator: Char, escape: String => String, unescape: String => String) {
    def unsafePrintPath(path: Path[_, _, _]): String = {
      val s: String = flatten("", ".", "..", escape, escape, path)
        .intercalate(separator.toString)

      maybeDir(path) match {
        case Some(_) => show"$s$separator"
        case None    => s
      }
    }

    def printPath[B, T](path: Path[B, T, Sandboxed]): String =
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

      def folder[B, S](base: Path[B, Dir, S], segments: String): Path[B, Dir, S] = segments match {
        case ""   => base
        case "."  => base
        case ".." => InternalParent(base)
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

    val parseRelFile: String => Option[RelativeFile[Unsandboxed]] =
      parsePath[Option[RelativeFile[Unsandboxed]]](Some(_), _ => None, _ => None, _ => None)

    val parseAbsFile: String => Option[AbsoluteFile[Unsandboxed]] =
      parsePath[Option[AbsoluteFile[Unsandboxed]]](_ => None, Some(_), _ => None, _ => None)

    val parseRelDir: String => Option[RelativeDir[Unsandboxed]] =
      parsePath[Option[RelativeDir[Unsandboxed]]](_ => None, _ => None, Some(_), _ => None)

    val parseAbsDir: String => Option[AbsoluteDir[Unsandboxed]] =
      parsePath[Option[AbsoluteDir[Unsandboxed]]](_ => None, _ => None, _ => None, Some(_))

    private def asDir[B, S](path: Path[B, File, S]): Path[B, Dir, S] = path match {
      case InternalFile(p, FileName(n)) => InternalDir(unsafeCoerceType(p), DirName(n))
      case _                            => sys.error("impossible!")
    }

    val parseRelAsDir: String => Option[RelativeDir[Unsandboxed]] =
      parsePath[Option[RelativeDir[Unsandboxed]]](p => Some(asDir(p)), _ => None, Some(_), _ => None)

    val parseAbsAsDir: String => Option[AbsoluteDir[Unsandboxed]] =
      parsePath[Option[AbsoluteDir[Unsandboxed]]](_ => None, p => Some(asDir(p)), _ => None, Some(_))

    implicit def encodePath[B, T, S]: Encoder[Path[B, T, S]] = Encoder.encodeString.contramap(p => unsafePrintPath(p))
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

  implicit def pathShow[B, T, S]: Show[Path[B, T, S]] = new Show[Path[B, T, S]] {
    override def show(t: Path[B, T, S]): String = t match {
      case Current                      => "currentDir"
      case Root                         => "rootDir"
      case InternalParent(p)            => show"parentDir($p)"
      case InternalDir(p, DirName(d))   => show"$p </> dir($d)"
      case InternalFile(p, FileName(f)) => show"$p </> file($f)"
    }
  }

}
