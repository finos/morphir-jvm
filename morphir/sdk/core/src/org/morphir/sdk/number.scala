package org.morphir.sdk

object number {

  def add[A: Numeric](left: A)(right: A)(implicit numeric: Numeric[A]): A =
    numeric.plus(left, right)

  def subtract[A](left: A)(right: A)(implicit numeric: Numeric[A]): A =
    numeric.minus(left, right)

  def multiply[A](left: A)(right: A)(implicit numeric: Numeric[A]): A =
    numeric.times(left, right)

  def abs[A](value: A)(implicit numeric: Numeric[A]): A =
    numeric.abs(value)

  def negate[A](value: A)(implicit numeric: Numeric[A]): A =
    numeric.negate(value)

}
