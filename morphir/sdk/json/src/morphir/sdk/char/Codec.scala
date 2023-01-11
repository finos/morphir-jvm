package morphir.sdk.char

import io.circe.{ Decoder, Encoder }

object Codec {
  def decodeChar: Decoder[Char] = Decoder.decodeChar

  def encodeChar: Encoder[Char] = Encoder.encodeChar

}
