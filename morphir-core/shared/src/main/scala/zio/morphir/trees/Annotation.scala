package zio.morphir.trees

import zio.*

final class Annotation[V] private (
    val identifier: String,
    val initial: V,
    val combine: (V, V) => V,
    private val tag: Tag[V]
) extends Serializable { self =>
  type Type = V

  override def equals(that: Any): Boolean = (that: @unchecked) match {
    case that: Annotation[_] => (identifier, tag) == ((that.identifier, that.tag))
  }

  private[trees] val unsafeCombine: (Any, Any) => V = combine.asInstanceOf[(Any, Any) => V]

  override lazy val hashCode: Int = (identifier, tag).hashCode
}

object Annotation {
  def apply[V](identifier: String, initial: V, combine: (V, V) => V)(implicit tag: Tag[V]): Annotation[V] =
    new Annotation(identifier, initial, combine, tag)

}
