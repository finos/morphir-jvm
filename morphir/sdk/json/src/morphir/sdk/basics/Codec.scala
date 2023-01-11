package morphir.sdk.basics

import io.circe.{ Decoder, Encoder }
import morphir.sdk.Basics.{ Bool, Decimal, Float, Int }

object Codec {

  /* Encoder / Decoder for Bool Types */
  implicit val encodeBool: Encoder[Bool] =
    Encoder.encodeBoolean

  implicit val decodeBool: Decoder[Bool] =
    Decoder.decodeBoolean

  /* Encoder / Decoder for Integer Types */
  implicit val encodeInt: Encoder[Int] =
    Encoder.encodeInt

  implicit val decodeInt: Decoder[Int] =
    Decoder.decodeInt

  /* Encoder / Decoder for Float Types */
  implicit val encodeFloat: Encoder[Float] =
    Encoder.encodeDouble

  implicit val decodeFloat: Decoder[Float] =
    Decoder.decodeDouble

  /* Encoder / Decoder for Unit Type */
  implicit val encodeUnit: Encoder[Unit] = _ => io.circe.Json.obj()

  implicit val decodeUnit: Decoder[Unit] = (_: io.circe.HCursor) => Right({})

}
