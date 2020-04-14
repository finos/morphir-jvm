package org.morphir.sdk.core
import String.String

object Bool {
  type Bool = Boolean

  val True: Bool = true
  val False: Bool = false

  @inline def and(a: Bool)(b: Bool): Bool = a && b
  @inline def not(value: Bool): Bool = !value
  @inline def or(a: Bool)(b: Bool): Bool = a || b
  @inline def xor(a: Bool)(b: Bool): Bool = a ^ b
  @inline def toString(value: Bool): String = value.toString
}
