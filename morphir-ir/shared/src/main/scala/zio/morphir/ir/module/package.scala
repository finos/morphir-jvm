package zio.morphir.ir

import scala.annotation.nowarn

package object module {

  @nowarn
  final case class ModulePath(toPath: Path) extends AnyVal {
    self =>

    def toModuleName: ModuleName = ModuleName.fromPath(toPath)

  }
  object ModulePath {
    def fromString(path: String): ModulePath = ModulePath(Path.fromString(path))
  }
}
