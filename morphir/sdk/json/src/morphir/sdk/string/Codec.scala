package morphir.sdk.string

import io.circe.{Decoder, Encoder}
import morphir.sdk.String.String

object Codec {

  implicit val decodeString : Decoder[String] =
    Decoder.decodeString

  implicit val encodeString : Encoder[String] =
    Encoder.encodeString

}
