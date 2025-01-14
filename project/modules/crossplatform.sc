import upickle.default.{ ReadWriter => RW, macroRW }

sealed trait Platform {
  def name: String
  override def toString = name
}
object Platform {
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

  def all: Set[Platform] = Set(JVM, JS, Native)
}
