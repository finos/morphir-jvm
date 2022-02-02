package zio.morphir.io
import zio.*
import zio.prelude.*
sealed trait VFilePath { self =>
  import VFilePath.*

  def /(child: VFilePath): VFilePath =
    (self, child) match {
      case (Root(_), Root(fileSeparator))                   => Root(fileSeparator)
      case (Root(_), RelativePath(segments, fileSeparator)) => AbsolutePath(segments, fileSeparator)
      case _                                                => ???
    }

  def dirname: VFilePath = {
    // val separator: String = fileSeparator
    self match {
      case root @ Root(_)                                      => root
      case AbsolutePath(NonEmptyList.Single(_), fileSeparator) => Root(fileSeparator)
      case path @ AbsolutePath(_, _) =>
        NonEmptyList.fromIterableOption(path.segments.dropRight(1)) match {
          case None      => Root(fileSeparator)
          case Some(lst) => AbsolutePath(lst, fileSeparator)
        }
      case path @ RelativePath(_, _) =>
        NonEmptyList.fromIterableOption(path.segments.dropRight(1)) match {
          case None      => RelativePath(".")(fileSeparator)
          case Some(lst) => RelativePath(lst, fileSeparator)
        }
    }
  }

  def fileSeparator: FileSeparator

  final def isAbsolute: Boolean = self match {
    case Root(_)            => true
    case AbsolutePath(_, _) => true
    case _                  => false
  }

  final def path: String = toString()

  def toList: List[String]
  final override def toString: String = self match {
    case Root(_)                => "/"
    case AbsolutePath(parts, _) => parts.mkString(fileSeparator, fileSeparator, "")
    case RelativePath(parts, _) => parts.mkString(fileSeparator)
  }
}

// final case class VFilePath private (private val segments: List[String]) { self =>
//   def /(segment: String): VFilePath  = VFilePath(segment :: segments)
//   def /(child: VFilePath): VFilePath = VFilePath(child.segments ++ segments)

//   def cwd: VFilePath = self match {
//     case VFilePath(Nil)          => self
//     case VFilePath(head :: tail) => VFilePath(tail)
//   }

//   def dirname: VFilePath = self match {
//     case VFilePath(Nil)          => self
//     case VFilePath(head :: tail) => VFilePath(head :: Nil)
//   }

//   def path: String = toString()

//   override def toString: String = segments.reverse.mkString("/")
// }
object VFilePath {
  val rootZIO: ZIO[VFileSystem, Nothing, VFilePath] = ZIO.serviceWith { case fs: VFileSystem => Root(fs.fileSeparator) }

  def root(implicit fileSeparator: FileSeparator): Root = Root(fileSeparator)
  def apply(path: String)(implicit fileSeparator: FileSeparator): VFilePath = {
    // val separator: String = fileSeparator
    path.split(fileSeparator).map(_.trim()).toList match {
      case Nil                  => Root(fileSeparator)
      case "" :: "" :: Nil      => Root(fileSeparator)
      case "" :: Nil            => Root(fileSeparator)
      case "" :: (head :: tail) => AbsolutePath(NonEmptyList(head, tail: _*), fileSeparator)
      case (head :: tail) if head.startsWith(fileSeparator) =>
        AbsolutePath(NonEmptyList(head, tail: _*), fileSeparator)
      case head :: tail => RelativePath(NonEmptyList(head, tail: _*), fileSeparator)
    }
  }

  def fromString(path: String)(implicit fileSeparator: predef.FileSeparator): VFilePath = apply(path)

  final case class AbsolutePath private[VFilePath] (segments: NonEmptyList[String], fileSeparator: FileSeparator)
      extends VFilePath {
    def toList: List[String] = segments.toList
  }
  object AbsolutePath {
    def apply(head: String, tail: String*)(implicit fileSeparator: FileSeparator): AbsolutePath =
      AbsolutePath(NonEmptyList(head, tail: _*), fileSeparator)
  }
  final case class RelativePath private[VFilePath] (segments: NonEmptyList[String], fileSeparator: FileSeparator)
      extends VFilePath {
    def toList: List[String] = segments.toList
  }
  object RelativePath {
    def apply(head: String, tail: String*)(implicit fileSeparator: FileSeparator): RelativePath =
      RelativePath(NonEmptyList(head, tail: _*), fileSeparator)
  }
  final case class Root(fileSeparator: FileSeparator) extends VFilePath {
    override def toList: List[String] = Nil
  }

}
