package millbuild.crossplatform

import upickle.default.{ ReadWriter => RW, macroRW }

sealed trait Platform extends Ordered[Platform] { self =>
  val isJS: Boolean        = self == Platform.JS
  val isJVM: Boolean       = self == Platform.JVM
  val isNative: Boolean    = self == Platform.Native
  val isNotNative: Boolean = !isNative
  def name: String
  def compare(that: Platform): Int = self.name.compare(that.name)

  def suffixes: Seq[String] = Seq(self.name) ++ pairs.map { case (a, b) =>
    a + "-" + b
  }
  final def pairs: Seq[(String, String)] =
    Platform.all
      .filter(_ != self)
      .map { p =>
        if (self < p) (self.name, p.name)
        else (p.name, self.name)
      }
      .toSeq

  override def toString = name
}
object Platform {
  lazy val all: Set[Platform] = Set(JVM, JS, Native)

  implicit val rw: RW[Platform] = RW.merge(
    macroRW[Platform.JVM],
    macroRW[Platform.JS],
    macroRW[Platform.Native]
  )
  type JVM = Platform.JVM.type
  case object JVM extends Platform {
    implicit val rw: RW[JVM.type] = macroRW
    def name                      = "jvm"
  }
  type JS = Platform.JS.type
  case object JS extends Platform {
    implicit val rw: RW[JS.type] = macroRW
    def name                     = "js"
  }
  type Native = Platform.Native.type
  case object Native extends Platform {
    implicit val rw: RW[Native.type] = macroRW
    def name                         = "native"
  }

  sealed trait FolderMode { self =>
    import FolderMode._

    final def useNesting: Boolean =
      self match {
        case UseBoth    => true
        case UseNesting => true
        case _          => false
      }

    final def useSuffix: Boolean =
      self match {
        case UseNesting => false
        case UseSuffix  => true
        case UseBoth    => true
      }

  }
  object FolderMode {
    implicit val rw: RW[FolderMode] = RW.merge(
      macroRW[UseNesting],
      macroRW[UseSuffix],
      macroRW[UseBoth]
    )
    type UseNesting = UseNesting.type
    case object UseNesting extends FolderMode {
      implicit val rn: RW[UseNesting] = macroRW
    }

    type UseSuffix = UseSuffix.type
    case object UseSuffix extends FolderMode {
      implicit val rw: RW[UseSuffix] = macroRW
    }

    type UseBoth = UseBoth.type
    case object UseBoth extends FolderMode {
      implicit val rw: RW[UseBoth] = macroRW
    }
  }

}
