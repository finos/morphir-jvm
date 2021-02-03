package morphir.flowz

import zio.duration._
import zio.{ Chunk, Fiber, Tag }

final class StepAnnotation[V] private (
  val identifier: String,
  val initial: V,
  val combine: (V, V) => V,
  private val tag: Tag[V]
) extends Serializable {

  override def equals(that: Any): Boolean = that match {
    case that: StepAnnotation[_] => (identifier, tag) == ((that.identifier, that.tag))
  }

  override lazy val hashCode: Int =
    (identifier, tag).hashCode
}

object StepAnnotation {
  def apply[V](identifier: String, initial: V, combine: (V, V) => V)(implicit tag: Tag[V]): StepAnnotation[V] =
    new StepAnnotation[V](identifier, initial, combine, tag)

  /**
   * An annotation which tags steps with strings.
   */
  val tagged: StepAnnotation[Set[String]] =
    StepAnnotation("tagged", Set.empty, _ union _)

  /**
   * An annotation for timing.
   */
  val timing: StepAnnotation[Duration] =
    StepAnnotation("timing", Duration.Zero, _ + _)

  import zio.Ref

  import scala.collection.immutable.SortedSet

  val fibers: StepAnnotation[Either[Int, Chunk[Ref[SortedSet[Fiber.Runtime[Any, Any]]]]]] =
    StepAnnotation("fibers", Left(0), compose)

  def compose[A](left: Either[Int, Chunk[A]], right: Either[Int, Chunk[A]]): Either[Int, Chunk[A]] =
    (left, right) match {
      case (Left(n), Left(m))           => Left(n + m)
      case (Right(refs1), Right(refs2)) => Right(refs1 ++ refs2)
      case (Right(_), Left(n))          => Left(n)
      case (Left(_), Right(refs))       => Right(refs)
    }
}
