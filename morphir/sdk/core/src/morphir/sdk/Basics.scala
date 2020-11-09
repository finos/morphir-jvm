/*
Copyright 2020 Morgan Stanley

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 */

package morphir.sdk

object Basics {

  type Bool = scala.Boolean

  // Equality
  @inline def equal[A](a: A)(b: A): Bool = a == b
  @inline def notEqual[A](a: A)(b: A): Bool = a != b

  // Comparable
  def lessThan[A: Ordering](a: A)(b: A): Bool = implicitly[Ordering[A]].lt(a, b)
  def lessThanOrEqual[A: Ordering](a: A)(b: A): Bool =
    implicitly[Ordering[A]].lteq(a, b)
  def greaterThan[A: Ordering](a: A)(b: A): Bool =
    implicitly[Ordering[A]].gt(a, b)
  def greaterThanOrEqual[A: Ordering](a: A)(b: A): Bool =
    implicitly[Ordering[A]].gteq(a, b)
  def min[A: Ordering](a: A)(b: A): A = if (lessThan(a)(b)) a else b
  def max[A: Ordering](a: A)(b: A): A = if (greaterThan(a)(b)) a else b

  // Int
  type Int = scala.Long
  def Int(v: scala.Long): Int = v

  @inline def add(a: Int)(b: Int): Int = a + b
  @inline def subtract(a: Int)(b: Int): Int = a - b
  @inline def multiply(a: Int)(b: Int): Int = a * b
  @inline def integerDivide(a: Int)(b: Int): Int = a / b

  // Float construction
  type Float = scala.Double
  @inline def Float(number: Number): Float =
    number.doubleValue()

  // Float functions
  @inline def add(a: Float)(b: Float): Float = a + b
  @inline def subtract(a: Float)(b: Float): Float = a - b
  @inline def multiply(a: Float)(b: Float): Float = a * b
  @inline def divide(a: Float)(b: Float): Float = a / b
  @inline def toFloat(a: Int): Float = a.toDouble
  @inline def round(a: Float): Int = a.round
  @inline def floor(a: Float): Int = a.floor.round
  @inline def ceiling(a: Float): Int = a.ceil.round
  @inline def truncate(a: Float): Int = if (a >= 0) floor(a) else -floor(-a)

  // Utilities
  @inline def identity[A](a: A): A = scala.Predef.identity(a)
  @inline def always[A, B](a: A)(b: B): A = a
  @inline def composeLeft[A, B, C](g: B => C)(f: A => B): A => C = a => g(f(a))
  @inline def composeRight[A, B, C](f: A => B)(g: B => C): A => C = a => g(f(a))
  def never[A](nothing: Nothing): A = nothing

  type Decimal = scala.BigDecimal

}
