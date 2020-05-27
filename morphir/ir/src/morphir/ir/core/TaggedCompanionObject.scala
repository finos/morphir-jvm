package morphir.ir.core

private[ir] abstract class TaggedCompanionObject(val Tag: String) {
  def hasMatchingTag[P <: Product](product: P): Boolean =
    if (product.productArity < 1) false
    else product.productElement(0) == Tag
}
