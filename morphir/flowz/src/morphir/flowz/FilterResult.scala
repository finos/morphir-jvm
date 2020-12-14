package morphir.flowz

import scala.annotation.nowarn

final case class FilterResult[+X, +I] private (included: Option[I], excluded: Option[X]) { self =>
  @nowarn
  private def copy(): Unit = ()

  def fold[A](ifExclude: X => A)(ifInclude: I => A): A =
    self match {
      case FilterResult(Some(value), None) => ifInclude(value)
      case FilterResult(None, Some(value)) => ifExclude(value)
      case other =>
        throw new IllegalArgumentException(
          s"Cannot match FilterResult variant for: $other. A filter result should have an included or excluded value."
        )
    }

  /**
   * Convert to an `Either` where `Excluded` results are `Left` and `Included` results are `Right`
   */
  def toEither: Either[X, I] = fold[Either[X, I]](Left(_))(Right(_))

  /**
   * Convert to an `Option` where the `Included` values are `Some`s and `Excluded` values are `None`
   */
  def toOption: Option[I] = included
}

object FilterResult {
  type Included[+A] = FilterResult[Nothing, A]
  type Excluded[+A] = FilterResult[A, Nothing]

  @nowarn
  private def apply[X, I](included: Option[I], excluded: Option[X]): FilterResult[Nothing, Nothing] = ???

  /**
   * Construct an excluded filter result.
   */
  def excluded[A](value: A): Excluded[A] = Excluded(value)

  /**
   * Construct an included filter result.
   */
  def included[A](value: A): Included[A] = Included(value)

  /**
   * Create a `FilterResult` from an `Either`
   */
  def fromEither[X, I](either: Either[X, I]): FilterResult[X, I] = either match {
    case Left(value)  => excluded(value)
    case Right(value) => included(value)
  }

  /**
   * Create a `FilterResult` from an `Option`
   */
  def fromOption[A](option: Option[A]): FilterResult[Option[Nothing], A] = option match {
    case Some(value) => included(value)
    case None        => excluded(None)
  }

  object Included {
    def apply[A](value: A): Included[A] =
      new FilterResult(included = Some(value), excluded = None)

    def unapply[X, A](result: FilterResult[X, A]): Option[A] =
      result match {
        case FilterResult(res @ Some(_), None) => res
        case _                                 => None
      }
  }

  object Excluded {
    def apply[A](value: A): Excluded[A] =
      new FilterResult(included = None, excluded = Some(value))

    def unapply[X, A](result: FilterResult[X, A]): Option[X] = result match {
      case FilterResult(None, res @ Some(_)) => res
      case _                                 => None
    }
  }

  implicit class FilterResultSyntax[A](self: FilterResult[A, A]) {
    def value: A = self.fold(identity)(identity)
  }

  implicit class IncludedSyntax[I](self: FilterResult[Nothing, I]) {
    def excludedAs[X]: FilterResult[X, I] = self
  }

  implicit class ExcludedSyntax[X](self: FilterResult[X, Nothing]) {
    def includedAs[I]: FilterResult[X, I] = self
  }
}
