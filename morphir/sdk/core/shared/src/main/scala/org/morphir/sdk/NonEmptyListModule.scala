package org.morphir.sdk

import org.morphir.sdk.NonEmptyListModule.NonEmptyList.{Cons, Single}

import scala.annotation.tailrec

object NonEmptyListModule {
  sealed trait NonEmptyList[+A] { self =>
    @tailrec
    final def foldLeft[B](z: B)(f: (B, A) => B): B =
      self match {
        case Cons(h, t) => t.foldLeft(f(z, h))(f)
        case Single(h)  => f(z, h)
      }
  }

  object NonEmptyList {
    final case class Cons[+A](head: A, tail: NonEmptyList[A])
        extends NonEmptyList[A]
    final case class Single[+A](head: A) extends NonEmptyList[A]
  }
}
