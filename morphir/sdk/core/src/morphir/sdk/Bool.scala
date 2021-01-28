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

import morphir.sdk.String.String

object Bool {
  type Bool = Boolean
  object Bool {
    @inline def apply(value: Boolean): Bool = value
  }

  val True: Bool  = true
  val False: Bool = false

  @inline def and(a: Bool)(b: Bool): Bool   = a && b
  @inline def not(value: Bool): Bool        = !value
  @inline def or(a: Bool)(b: Bool): Bool    = a || b
  @inline def xor(a: Bool)(b: Bool): Bool   = a ^ b
  @inline def toString(value: Bool): String = value.toString

  // Equality
  @inline def equal[A](a: A)(b: A): Bool    = a == b
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
}
