package morphir.sdk.char

import io.circe.{ Decoder, Encoder }

object Codec {
  implicit val decodeChar: Decoder[morphir.sdk.Char.Char] = (c: io.circe.HCursor) =>
    Decoder.decodeChar(c).map(morphir.sdk.Char.from)

  implicit val encodeChar: Encoder[morphir.sdk.Char.Char] = (char: morphir.sdk.Char.Char) =>
    Encoder.encodeString(char.toString)

}
