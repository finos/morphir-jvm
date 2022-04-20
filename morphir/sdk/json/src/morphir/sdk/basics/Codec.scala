package morphir.sdk.basics

import morphir.sdk.Basics.Int
import io.circe.{ Decoder, Encoder }

object Codec {

  implicit val decodeInt: Decoder[Int] =
    Decoder.decodeInt

  implicit val encodeInt: Encoder[Int] =
    Encoder.encodeInt

}
