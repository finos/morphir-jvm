package morphir.ir.typeclass.instances

import cats.Show
import morphir.ir.Path

trait PathInstances {
  implicit val showPath: Show[Path] =
    Show.show(path => path.value.mkString("[", ",", "]"))
}
