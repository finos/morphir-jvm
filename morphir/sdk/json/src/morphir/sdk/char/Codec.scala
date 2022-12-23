package morphir.sdk.char

import io.circe.{ Decoder, Encoder }

object Codec {
  def encodeChar: Encoder[morphir.sdk.Char.Char] = ???

  def decodeChar: Decoder[morphir.sdk.Char.Char] = ???
}
