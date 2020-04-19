package org.morphir.sdk

import org.morphir.sdk.Maybe.Maybe

object String {
  type String = scala.Predef.String

  @inline def fromInt(int: Int): String =
    int.toString

  @inline def toInt(text: String): Maybe[Int] =
    StringOps.toInt(text)
}
