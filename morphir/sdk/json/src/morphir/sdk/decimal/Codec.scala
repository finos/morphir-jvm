package morphir.sdk.decimal

import io.circe.{ Decoder, Encoder }

object Codec {
  def encodeDecimal: Encoder[morphir.sdk.Decimal.Decimal] = ???
  def decodeDecimal: Decoder[morphir.sdk.Decimal.Decimal] = ???
}
