package zio.morphir.ir

import scala.annotation.implicitNotFound
import scala.util.NotGiven

sealed abstract class IsNotAValue[-A] extends Serializable

object IsNotAValue extends IsNotAValue[Any] with IsNotAValueLowerPriority {
  implicit def isNotAValue[A](using NotGiven[A <:< value.Value[_, _]]): IsNotAValue[A] = IsNotAValue

}

trait IsNotAValueLowerPriority {
  implicit def isNotARecursiveValue[A](using NotGiven[A <:< value.recursive.Value[_, _]]): IsNotAValue[A] = IsNotAValue
}
