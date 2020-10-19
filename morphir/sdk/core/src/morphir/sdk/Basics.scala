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
  type Decimal = scala.BigDecimal

  // Float construction
  type Float   = scala.Double
  @inline def Float(number: Number): Float =
    number.doubleValue()

  // Float functions
  @inline def add(f1: Float)(f2: Float): Float =
    ( f1 + f2 )
  @inline def subtract(f1: Float)(f2: Float): Float =
    ( f1 - f2 )
  @inline def multiply(f1: Float)(f2: Float): Float =
    ( f1 * f2 )
  @inline def divide(f1: Float)(f2: Float): Float =
    ( f1 / f2 )
  @inline def power(f1: Float)(f2: Float): Float =
    Math.pow( f1, f2 )
  @inline def equal(f1: Float)(f2: Float): Boolean =
    ( f1 == f2 )
  @inline def notEqual(f1: Float)(f2: Float): Boolean =
    ( f1 != f2 )
  @inline def lessThan(f1: Float)(f2: Float): Boolean =
    ( f1 < f2 )
  @inline def greaterThan(f1: Float)(f2: Float): Boolean =
    ( f1 > f2 )
  @inline def lessThanOrEqual(f1: Float)(f2: Float): Boolean =
    ( f1 <= f2 )
  @inline def greaterThanOrEqual(f1: Float)(f2: Float): Boolean =
    ( f1 >= f2 )
}
