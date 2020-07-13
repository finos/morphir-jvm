package morphir.sdk

import morphir.sdk.Maybe.Maybe

import scala.util.Try

object StringOps {
  @inline def toInt(text: String): Maybe[Int] =
    Try(text.toInt)
}
