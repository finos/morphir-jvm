package zio.morphir.ir

import scala.annotation.implicitNotFound
import scala.util.NotGiven

/**
 * A value of type `NeedAttributes[A]` provides implicit evidence that a node with attributes type `A` needs attribute
 * values, that is, that `A` is not equal to `Any`.
 */
@implicitNotFound(
  "This operation assumes that your node requires attributes. " +
    "However, your node has Any for the attributes type, which means it " +
    "requires no attributes, so there is no need to provide attributes to the node."
)
sealed abstract class NeedsAttributes[+A] extends Serializable

object NeedsAttributes extends NeedsAttributes[Nothing] {
  implicit def needsAttributes[A](using NotGiven[A =:= Any]): NeedsAttributes[A] = NeedsAttributes
}
