package org.morphir.sdk

import org.morphir.sdk.Maybe._
import org.morphir.sdk.String.String
import scala.util.Try

object StringOps {
  @inline def toInt(text: String): Maybe[Int] =
    Try(text.toInt)
}
