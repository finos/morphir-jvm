package morphir.sdk.json

import upickle.default.{readwriter, ReadWriter => RW, macroRW}
import upickle.default._

object Decode {
  type Value = ujson.Value
}
