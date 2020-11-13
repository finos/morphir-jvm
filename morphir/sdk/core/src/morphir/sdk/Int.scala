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

object Int {
  type Int   = scala.BigInt
  type Int8  = scala.Byte
  type Int16 = scala.Short
  type Int32 = scala.Int
  type Int64 = scala.Long

  @inline def divide(dividend: Int)(divisor: Int): Int       = dividend / divisor
  @inline def divide(dividend: Int8)(divisor: Int8): Int8    =
    (dividend / divisor).toByte
  @inline def divide(dividend: Int16)(divisor: Int16): Int16 =
    (dividend / divisor).toShort
  @inline def divide(dividend: Int32)(divisor: Int32): Int32 =
    dividend / divisor
  @inline def divide(dividend: Int64)(divisor: Int64): Int64 =
    dividend / divisor

  @inline def modBy(divisor: Int)(dividend: Int): Int    = (dividend % divisor).abs
  @inline def modBy(divisor: Int8)(dividend: Int8): Int8 =
    (dividend % divisor).toByte.abs
  @inline def modBy(divisor: Int16)(dividend: Int16): Int16 =
    (dividend % divisor).toShort.abs
  @inline def modBy(divisor: Int32)(dividend: Int32): Int32 =
    (dividend % divisor).abs
  @inline def modBy(divisor: Int64)(dividend: Int64): Int64 =
    (dividend % divisor).abs

  @inline def remainderBy(divisor: Int)(dividend: Int): Int    = dividend % divisor
  @inline def remainderBy(divisor: Int8)(dividend: Int8): Int8 =
    (dividend % divisor).toByte
  @inline def remainderBy(divisor: Int16)(dividend: Int16): Int16 =
    (dividend % divisor).toShort
  @inline def remainderBy(divisor: Int32)(dividend: Int32): Int32 =
    dividend % divisor
  @inline def remainderBy(divisor: Int64)(dividend: Int64): Int64 =
    dividend % divisor

}
