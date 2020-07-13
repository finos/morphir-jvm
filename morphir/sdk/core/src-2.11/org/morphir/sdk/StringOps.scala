package org.morphir.sdk

import org.morphir.sdk.maybe._
import org.morphir.sdk.string.String
import scala.util.Try

object StringOps {
  @inline def toInt(text: String): Maybe[Int] =
    Try(text.toInt)
}
