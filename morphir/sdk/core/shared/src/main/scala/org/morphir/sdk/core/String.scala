package org.morphir.sdk.core

object String {
  @inline def fromInt(int: Int): String =
    int.toString()
}
