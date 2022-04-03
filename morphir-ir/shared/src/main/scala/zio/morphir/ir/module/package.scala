package zio.morphir.ir

import scala.annotation.nowarn

package object module {

  @nowarn
  final case class ModulePath(toPath: Path) extends AnyVal
}
