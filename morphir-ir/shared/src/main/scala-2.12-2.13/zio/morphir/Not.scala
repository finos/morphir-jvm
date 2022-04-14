package zio.morphir

import scala.annotation.implicitAmbiguous

/**
 * Provides implicit evidence that an instance of `A` is not in implicit scope.
 */
@implicitAmbiguous("Implicit ${A} defined.")
sealed trait Not[A]

object Not {

  /**
   * Derives a `Not[A]` instance when an instance of `A` is not in implciit scope.
   */
  implicit def Not[A]: Not[A] =
    new Not[A] {}

  /**
   * Derives a `Not[A]` instance when an instance of `A` is in implicit scope. Together with the instance defined below
   * this will cause implicit search to fail due to ambiguous implicits, preventing an instance of `Not[A]` from being
   * in implicit scope when an instance of `A` is in implicit scope.
   */
  implicit def NotAmbiguous1[A](implicit ev: A): Not[A] =
    new Not[A] {}

  /**
   * Derives a `Not[A]` instance when an instance of `A` is in implicit scope. Together with the instance defined above
   * this will cause implicit search to fail due to ambiguous implicits, preventing an instance of `Not[A]` from being
   * in implicit scope when an instance of `A` is in implicit scope.
   */
  implicit def NotAmbiguous2[A](implicit ev: A): Not[A] =
    new Not[A] {}
}
