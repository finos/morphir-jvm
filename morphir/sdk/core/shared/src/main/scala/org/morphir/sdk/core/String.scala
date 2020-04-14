package org.morphir.sdk.core

object String {
  type String = scala.Predef.String

  @inline def fromInt(int: Int): String =
    int.toString()
}
