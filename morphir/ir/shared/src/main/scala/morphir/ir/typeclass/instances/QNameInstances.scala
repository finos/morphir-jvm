package morphir.ir.typeclass.instances

import cats.Show
import cats.implicits._

trait QNameInstances {
  implicit val showPath: Show[morphir.ir.QName] =
    Show.show[morphir.ir.QName](qn => show"${qn.modulePath}.${qn.localName}")
}
