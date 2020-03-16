package morphir.sdk.core
sealed abstract class Result[+E, +A] extends Product with Serializable {

  def isOk: Boolean

  def isErr: Boolean

  def flatMap[A1, E1 >: E](fn: A => Result[E1, A1]): Result[E1, A1]

  def getOrElse[A1 >: A](fallbackValue: A1): A1

  def map[A1](fn: A => A1): Result[E, A1]

  def mapError[E1 >: E](fn: E => E1): Result[E1, A]

}

object Result {

  case class Ok[E, A](value: A) extends Result[E, A] {

    def isOk: Boolean = true

    def isErr: Boolean = false

    def flatMap[A1, E1 >: E](fn: A => Result[E1, A1]): Result[E1, A1] =
      fn(value)

    def getOrElse[A1 >: A](fallbackValue: A1): A1 = value

    def map[B](fn: A => B): Result[E, B] =
      Ok(fn(value))

    def mapError[E1 >: E](fn: E => E1): Result[E1, A] =
      this.asInstanceOf[Result[E1, A]]
  }

  case class Err[E, A](error: E) extends Result[E, A] {

    def isOk: Boolean = false

    def isErr: Boolean = true

    def flatMap[A1, E1 >: E](fn: A => Result[E1, A1]): Result[E1, A1] =
      error.asInstanceOf[Result[E, A1]]

    def getOrElse[A1 >: A](fallbackValue: A1): A1 = fallbackValue

    def map[A1](fn: A => A1): Result[E, A1] =
      error.asInstanceOf[Result[E, A1]]

    def mapError[E1 >: E](fn: E => E1): Result[E1, A] =
      Err(fn(error))
  }

  def andThen[E, A, B](fn: A => Result[E, B]): Result[E, A] => Result[E, B] =
    (result: Result[E, A]) => result.flatMap(fn)

  def map[E, A, A1](fn: A => A1): Result[E, A] => Result[E, A1] =
    (result: Result[E, A]) => result.map(fn)

  def map2[E, A, B, V](
      fn: A => B => V
  ): Result[E, A] => Result[E, B] => Result[E, V] =
    (resA: Result[E, A]) =>
      (resB: Result[E, B]) =>
        (resA, resB) match {
          case (Ok(a), Ok(b))    => Ok(fn(a)(b))
          case (err @ Err(_), _) => err.asInstanceOf[Result[E, V]]
          case (_, err @ Err(_)) => err.asInstanceOf[Result[E, V]]
        }

  def map3[E, A, B, C, V](
      fn: A => B => C => V
  ): Result[E, A] => Result[E, B] => Result[E, C] => Result[E, V] =
    (resA: Result[E, A]) =>
      (resB: Result[E, B]) =>
        (resC: Result[E, C]) =>
          (resA, resB, resC) match {
            case (Ok(a), Ok(b), Ok(c)) => Ok(fn(a)(b)(c))
            case (err @ Err(_), _, _)  => err.asInstanceOf[Result[E, V]]
            case (_, err @ Err(_), _)  => err.asInstanceOf[Result[E, V]]
            case (_, _, err @ Err(_))  => err.asInstanceOf[Result[E, V]]
          }

  def map4[E, A, B, C, D, V](
      fn: A => B => C => D => V
  ): Result[E, A] => Result[E, B] => Result[E, C] => Result[E, D] => Result[
    E,
    V
  ] =
    (resA: Result[E, A]) =>
      (resB: Result[E, B]) =>
        (resC: Result[E, C]) =>
          (resD: Result[E, D]) =>
            (resA, resB, resC, resD) match {
              case (Ok(a), Ok(b), Ok(c), Ok(d)) => Ok(fn(a)(b)(c)(d))
              case (err @ Err(_), _, _, _)      => err.asInstanceOf[Result[E, V]]
              case (_, err @ Err(_), _, _)      => err.asInstanceOf[Result[E, V]]
              case (_, _, err @ Err(_), _)      => err.asInstanceOf[Result[E, V]]
              case (_, _, _, err @ Err(_))      => err.asInstanceOf[Result[E, V]]
            }

  def map5[E, A1, A2, A3, A4, A5, V](
      fn: A1 => A2 => A3 => A4 => A5 => V
  ): Result[E, A1] => Result[E, A2] => Result[E, A3] => Result[E, A4] => Result[
    E,
    A5
  ] => Result[E, V] =
    (resA: Result[E, A1]) =>
      (resB: Result[E, A2]) =>
        (resC: Result[E, A3]) =>
          (resD: Result[E, A4]) =>
            (resE: Result[E, A5]) =>
              (resA, resB, resC, resD, resE) match {
                case (Ok(a), Ok(b), Ok(c), Ok(d), Ok(e)) =>
                  Ok(fn(a)(b)(c)(d)(e))
                case (err @ Err(_), _, _, _, _) =>
                  err.asInstanceOf[Result[E, V]]
                case (_, err @ Err(_), _, _, _) =>
                  err.asInstanceOf[Result[E, V]]
                case (_, _, err @ Err(_), _, _) =>
                  err.asInstanceOf[Result[E, V]]
                case (_, _, _, err @ Err(_), _) =>
                  err.asInstanceOf[Result[E, V]]
                case (_, _, _, _, err @ Err(_)) =>
                  err.asInstanceOf[Result[E, V]]
              }

  def toMaybe[E, A](result: Result[E, A]): Maybe[A] =
    result match {
      case Ok(value) => Maybe.just(value)
      case _         => Maybe.nothing
    }
}
