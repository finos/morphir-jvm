package org.morphir.sdk

import org.morphir.sdk.Maybe.Maybe
import org.morphir.sdk.String.String

object StringOps {
  @inline def toInt(text: String): Maybe[Int] =
    text.toIntOption
}
