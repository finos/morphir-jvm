package org.morphir.sdk.core

import scala.language.implicitConversions

object Maybe {

  sealed abstract class Maybe[+A] extends Product with Serializable {

    def get: A

    def map[B](fn: A => B): Maybe[B] = this match {
      case Just(value) => Just(fn(value))
      case Nothing     => Nothing
    }
  }

  case class Just[+A](value: A) extends Maybe[A] {
    def get: A = value
  }

  case object Nothing extends Maybe[scala.Nothing] {
    def get: scala.Nothing = throw new NoSuchElementException("Nothing.get")
  }

  val nothing: Maybe[scala.Nothing] = Nothing

  def just[A](value: A): Maybe[A] = Some(value)

  def map[A, A1](fn: A => A1): Maybe[A] => Maybe[A1] =
    (value: Maybe[A]) =>
      value match {
        case Just(a) => Just(fn(a))
        case _       => Nothing.asInstanceOf[Maybe[A1]]
      }

  def map2[A, B, V](fn: A => B => V): Maybe[A] => Maybe[B] => Maybe[V] =
    (maybeA: Maybe[A]) =>
      (maybeB: Maybe[B]) =>
        (maybeA, maybeB) match {
          case (Just(a), Just(b)) => Just(fn(a)(b))
          case _                  => Nothing.asInstanceOf[Maybe[V]]
        }

  def map3[A, B, C, V](
      fn: A => B => C => V
  ): Maybe[A] => Maybe[B] => Maybe[C] => Maybe[V] =
    (maybeA: Maybe[A]) =>
      (maybeB: Maybe[B]) =>
        (maybeC: Maybe[C]) =>
          (maybeA, maybeB, maybeC) match {
            case (Just(a), Just(b), Just(c)) => Just(fn(a)(b)(c))
            case _                           => Nothing.asInstanceOf[Maybe[V]]
          }

  def map4[A, B, C, D, V](
      fn: A => B => C => D => V
  ): Maybe[A] => Maybe[B] => Maybe[C] => Maybe[D] => Maybe[V] =
    (maybeA: Maybe[A]) =>
      (maybeB: Maybe[B]) =>
        (maybeC: Maybe[C]) =>
          (maybeD: Maybe[D]) =>
            (maybeA, maybeB, maybeC, maybeD) match {
              case (Just(a), Just(b), Just(c), Just(d)) => Just(fn(a)(b)(c)(d))
              case _                                    => Nothing.asInstanceOf[Maybe[V]]
            }

  def map5[A, B, C, D, E, V](
      fn: A => B => C => D => E => V
  ): Maybe[A] => Maybe[B] => Maybe[C] => Maybe[D] => Maybe[E] => Maybe[V] =
    (maybeA: Maybe[A]) =>
      (maybeB: Maybe[B]) =>
        (maybeC: Maybe[C]) =>
          (maybeD: Maybe[D]) =>
            (maybeE: Maybe[E]) =>
              (maybeA, maybeB, maybeC, maybeD, maybeE) match {
                case (Just(a), Just(b), Just(c), Just(d), Just(e)) =>
                  Just(fn(a)(b)(c)(d)(e))
                case _ => Nothing.asInstanceOf[Maybe[V]]
              }

  def andThen[A, B](fn: A => Maybe[B]): Maybe[A] => Maybe[B] =
    (maybeA: Maybe[A]) =>
      maybeA match {
        case Just(value) => fn(value)
        case Nothing     => Nothing.asInstanceOf[Maybe[B]]
      }

  def withDefault[A, A1 >: A](defaultValue: A1) =
    (maybeValue: Maybe[A]) =>
      maybeValue match {
        case _: Maybe.Nothing.type => defaultValue
        case Just(value)           => value
      }

  implicit def toOption[A](maybe: Maybe[A]): Option[A] = maybe match {
    case Just(value) => Some(value)
    case Nothing     => None
  }

  implicit def fromOption[A](option: Option[A]): Maybe[A] = option match {
    case Some(value) => Just(value)
    case None        => Nothing
  }
}
