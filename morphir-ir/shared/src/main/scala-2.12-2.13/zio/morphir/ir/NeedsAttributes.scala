package zio.morphir.ir

import scala.annotation.implicitAmbiguous

/**
 * A value of type `NeedAttributes[A]` provides implicit evidence that a node with attributes type `A` needs attribute
 * values, that is, that `A` is not equal to `Any`.
 */
sealed abstract class NeedsAttributes[+A] extends Serializable

object NeedsAttributes extends NeedsAttributes[Nothing] {
  implicit def needsAttributes[A]: NeedsAttributes[A] = NeedsAttributes

  // Provide multiple ambiguous values so an implicit NeedsAtributes[Any] cannot be found.
  @implicitAmbiguous(
    "This operation assumes that your node requires attributes. " +
      "However, your node has Any for the attributes type, which means it " +
      "requires no attributes, so there is no need to provide attributes to the node."
  )
  implicit val needsAttributesAmbiguous1: NeedsAttributes[Any] = NeedsAttributes
  implicit val needsAttributesAmbiguous2: NeedsAttributes[Any] = NeedsAttributes
}
