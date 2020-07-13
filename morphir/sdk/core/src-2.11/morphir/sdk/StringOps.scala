package morphir.sdk

import morphir.sdk.Maybe._
import morphir.sdk.String.String

import scala.util.Try

object StringOps {
  @inline def toInt(text: String): Maybe[Int] =
    Try(text.toInt)
}
