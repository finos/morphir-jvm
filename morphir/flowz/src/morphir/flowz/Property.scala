package morphir.flowz

import zio.duration._
import zio.{ Chunk, Fiber, Tag }

/**
 * Represents a named data holder which represents a property on some object.
 */
final class Property[V] private (
  val identifier: String,
  val initial: V,
  val combine: (V, V) => V,
  private val tag: Tag[V]
) extends Serializable {

  override def equals(that: Any): Boolean = that match {
    case that: Property[_] => (identifier, tag) == ((that.identifier, that.tag))
  }

  override lazy val hashCode: Int =
    (identifier, tag).hashCode
}

object Property {
  def apply[V](identifier: String, initial: V, combine: (V, V) => V)(implicit tag: Tag[V]): Property[V] =
    new Property[V](identifier, initial, combine, tag)

  /**
   * An annotation which tags steps with strings.
   */
  val tagged: Property[Set[String]] =
    Property("tagged", Set.empty, _ union _)

  /**
   * An annotation for timing.
   */
  val timing: Property[Duration] =
    Property("timing", Duration.Zero, _ + _)

  import zio.Ref

  import scala.collection.immutable.SortedSet

  val fibers: Property[Either[Int, Chunk[Ref[SortedSet[Fiber.Runtime[Any, Any]]]]]] =
    Property("fibers", Left(0), compose)

  def compose[A](left: Either[Int, Chunk[A]], right: Either[Int, Chunk[A]]): Either[Int, Chunk[A]] =
    (left, right) match {
      case (Left(n), Left(m))           => Left(n + m)
      case (Right(refs1), Right(refs2)) => Right(refs1 ++ refs2)
      case (Right(_), Left(n))          => Left(n)
      case (Left(_), Right(refs))       => Right(refs)
    }
}
