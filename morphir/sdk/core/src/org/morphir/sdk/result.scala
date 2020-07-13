package org.morphir.sdk

import maybe._

sealed abstract class result[+E, +A] extends Product with Serializable {

  def isOk: Boolean

  def isErr: Boolean

  def flatMap[A1, E1 >: E](fn: A => result[E1, A1]): result[E1, A1] =
    this match {
      case result.Ok(value) => fn(value)
      case _                => this.asInstanceOf[result[E, A1]]
    }

  def getOrElse[A1 >: A](fallbackValue: A1): A1

  def map[A1](fn: A => A1): result[E, A1] =
    this match {
      case result.Ok(value) => result.Ok(fn(value))
      case _                => this.asInstanceOf[result[E, A1]]
    }

  def mapError[E1 >: E](fn: E => E1): result[E1, A]

}

object result {

  case class Ok[+E, +A](value: A) extends result[E, A] {

    def isOk: Boolean = true

    def isErr: Boolean = false

    def getOrElse[A1 >: A](fallbackValue: A1): A1 = value

    def mapError[E1 >: E](fn: E => E1): result[E1, A] =
      this.asInstanceOf[result[E1, A]]

    def withErr[E1 >: E]: result[E1, A] = this
  }

  case class Err[+E, +A](error: E) extends result[E, A] {

    def isOk: Boolean = false

    def isErr: Boolean = true

    def getOrElse[A1 >: A](fallbackValue: A1): A1 = fallbackValue

    def mapError[E1 >: E](fn: E => E1): result[E1, A] =
      Err(fn(error))

    def withOk[A1 >: A]: result[E, A1] = this
  }

  def ok[A](value: A): result[Nothing, A] = Ok(value)

  def err[E](error: E): result[E, Nothing] = Err(error)

  def andThen[E, A, B](fn: A => result[E, B])(result: result[E, A]): result[E, B] =
    result.flatMap(fn)

  def map[X, A, V](fn: A => V)(result: result[X, A]): result[X, V] =
    result.map(fn)

  def map2[X, A, B, V](fn: (A, B) => V)(resA: result[X, A])(resB: result[X, B]): result[X, V] =
    (resA, resB) match {
      case (Ok(a), Ok(b))    => Ok(fn(a, b))
      case (err @ Err(_), _) => err.asInstanceOf[result[X, V]]
      case (_, err @ Err(_)) => err.asInstanceOf[result[X, V]]
    }

  def map3[X, A, B, C, V](
    fn: (A, B, C) => V
  )(resA: result[X, A])(resB: result[X, B])(resC: result[X, C]): result[X, V] =
    (resA, resB, resC) match {
      case (Ok(a), Ok(b), Ok(c)) => Ok(fn(a, b, c))
      case (err @ Err(_), _, _)  => err.asInstanceOf[result[X, V]]
      case (_, err @ Err(_), _)  => err.asInstanceOf[result[X, V]]
      case (_, _, err @ Err(_))  => err.asInstanceOf[result[X, V]]
    }

  def map4[X, A, B, C, D, V](
    fn: (A, B, C, D) => V
  )(resA: result[X, A])(resB: result[X, B])(resC: result[X, C])(resD: result[X, D]): result[X, V] =
    (resA, resB, resC, resD) match {
      case (Ok(a), Ok(b), Ok(c), Ok(d)) => Ok(fn(a, b, c, d))
      case (err @ Err(_), _, _, _)      => err.asInstanceOf[result[X, V]]
      case (_, err @ Err(_), _, _)      => err.asInstanceOf[result[X, V]]
      case (_, _, err @ Err(_), _)      => err.asInstanceOf[result[X, V]]
      case (_, _, _, err @ Err(_))      => err.asInstanceOf[result[X, V]]
    }

  def map5[X, A, B, C, D, E, V](
    fn: (A, B, C, D, E) => V
  )(resA: result[X, A])(resB: result[X, B])(resC: result[X, C])(resD: result[X, D])(resE: result[X, E]): result[X, V] =
    (resA, resB, resC, resD, resE) match {
      case (Ok(a), Ok(b), Ok(c), Ok(d), Ok(e)) => Ok(fn(a, b, c, d, e))
      case (err @ Err(_), _, _, _, _)          => err.asInstanceOf[result[X, V]]
      case (_, err @ Err(_), _, _, _)          => err.asInstanceOf[result[X, V]]
      case (_, _, err @ Err(_), _, _)          => err.asInstanceOf[result[X, V]]
      case (_, _, _, err @ Err(_), _)          => err.asInstanceOf[result[X, V]]
      case (_, _, _, _, err @ Err(_))          => err.asInstanceOf[result[X, V]]
    }

  def mapError[E, E1, A](fn: E => E1): result[E, A] => result[E1, A] = {
    case Err(error) => Err(fn(error))
    case result     => result.asInstanceOf[result[E1, A]]
  }

  def toMaybe[E, A](result: result[E, A]): Maybe[A] =
    result match {
      case Ok(value) => maybe.just(value)
      case _         => maybe.nothing
    }

  def fromMaybe[E, A](errorValue: => E): Maybe[A] => result[E, A] = {
    case maybe.Just(value) => result.Ok(value)
    case maybe.Nothing     => result.Err(errorValue)
  }

  def fromOption[E, A](errorValue: => E): Option[A] => result[E, A] = {
    case Some(value) => result.Ok(value)
    case None        => result.Err(errorValue)
  }

  def fromEither[E, A](either: Either[E, A]): result[E, A] =
    either match {
      case Left(err)    => result.Err(err)
      case Right(value) => result.Ok(value)
    }

  implicit def resultFromEither[E, A](either: Either[E, A]): result[E, A] =
    either match {
      case Left(err)    => result.Err(err)
      case Right(value) => result.Ok(value)
    }

  def unit[E]: result[E, Unit] = result.Ok(())
}
