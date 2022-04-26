package morphir.sdk.localdate

import io.circe.{Decoder, Encoder}
import morphir.sdk.LocalDate.LocalDate

object Codec {
  implicit val encodeLocalDate: Encoder[LocalDate] =
    Encoder.encodeLocalDate

  implicit val decodeLocalDate: Decoder[LocalDate] =
    Decoder.decodeLocalDate
}
