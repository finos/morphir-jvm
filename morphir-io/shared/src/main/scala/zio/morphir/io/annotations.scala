package zio.morphir.io
import zio.*

final class AnnotationMap private (private val map: Map[Annotation[Any], AnyRef]) {
  self =>

  def ++(that: AnnotationMap): AnnotationMap =
    new AnnotationMap((self.map.toVector ++ that.map.toVector).foldLeft[Map[Annotation[Any], AnyRef]](Map()) {
      case (acc, (key, value)) =>
        acc + (key -> acc.get(key).fold(value)(key.combine(_, value).asInstanceOf[AnyRef]))
    })

  /**
   * Appends the specified annotation to the annotation map.
   */
  def annotate[V](key: Annotation[V], value: V): AnnotationMap = {
    val res = update[V](key, key.combine(_, value))
    res
  }

  /**
   * Retrieves the annotation of the specified type, or its default value if there is none.
   */
  def get[V](key: Annotation[V]): V =
    map.get(key.asInstanceOf[Annotation[Any]]).fold(key.initial)(_.asInstanceOf[V])

  private def overwrite[V](key: Annotation[V], value: V): AnnotationMap =
    new AnnotationMap(map + (key.asInstanceOf[Annotation[Any]] -> value.asInstanceOf[AnyRef]))

  private def update[V](key: Annotation[V], f: V => V): AnnotationMap =
    overwrite(key, f(get(key)))

  override def toString: String =
    map.toString
}

object AnnotationMap {
  val empty: AnnotationMap = new AnnotationMap(Map())
}

final class Annotation[V] private (
    val identifier: String,
    val initial: V,
    val combine: (V, V) => V,
    private val tag: Tag[V]
) extends Serializable { self =>
  override def equals(that: Any): Boolean = (that: @unchecked) match {
    case that: Annotation[_] => (identifier, tag) == ((that.identifier, that.tag))
  }

  override lazy val hashCode: Int = (identifier, tag).hashCode
}

object Annotation {
  def apply[V](identifier: String, initial: V, combine: (V, V) => V)(implicit tag: Tag[V]): Annotation[V] =
    new Annotation(identifier, initial, combine, tag)

}

// final case class Properties[Case[+_], +Props](env: ZEnvironment[Props]) {}
//
// sealed trait Property {}
//
// object Property {
//   case class LineCount(count: Int) extends Property
//   case class FileCount(count: Int) extends Property
// }
// object Properties {
//
//   def lines(count: Int) = ZEnvironment.empty.add(Property.LineCount(count))
//   def file(count: Int)  = ZEnvironment.empty.add(Property.FileCount(count))
// }
//
