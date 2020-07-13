package org.morphir.sdk

import org.morphir.sdk.Bool.Bool
import org.morphir.sdk.Maybe.Maybe
import org.morphir.sdk.List.List

object rule {

  type Rule[A, B] = A => Maybe[B]

  def chain[A, B](rules: List[Rule[A, B]]): Rule[A, B] =
    input =>
      rules
        .find(rule => rule(input).isDefined)
        .flatMap(rule => rule(input))

  def any[A]: A => Bool =
    _ => Bool.True

  def is[A](ref: A)(input: A): Bool =
    ref == input

  def anyOf[A](ref: List[A])(input: A): Bool =
    ref.contains(input)

  def noneOf[A](ref: List[A])(input: A): Bool =
    !anyOf(ref)(input)

}
