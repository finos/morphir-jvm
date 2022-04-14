package zio.morphir
import scala.annotation.implicitNotFound
import scala.util.NotGiven

/**
 * Provides implicit evidence that an instance of `A` is not in implicit scope.
 */
@implicitNotFound("Implicit ${A} defined.")
sealed trait Not[A]
object Not {

  /**
   * Derives a `Not[A]` instance from a `NotGiven[A]` instance.
   */
  implicit def Not[A: NotGiven]: Not[A] =
    new Not[A] {}
}
