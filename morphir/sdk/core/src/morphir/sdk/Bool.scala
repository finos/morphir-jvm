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
}
