package morphir.sdk.decimal

import io.circe.{Decoder, Encoder}
import morphir.sdk.Basics.Decimal

object Codec {
  /* Encoder / Decoder for Decimal Types */
  implicit val encodeDecimal: Encoder[Decimal] =
    Encoder.encodeBigDecimal

  implicit val decodeDecimal: Decoder[Decimal] =
    Decoder.decodeBigDecimal
}
