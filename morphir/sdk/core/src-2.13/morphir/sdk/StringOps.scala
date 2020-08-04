package morphir.sdk

import morphir.sdk.Maybe.Maybe
import morphir.sdk.String.String

object StringOps {
  @inline def toInt(text: String): Maybe[Int] =
    text.toIntOption
}
