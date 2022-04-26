package morphir.sdk.maybe

import io.circe.{ Decoder, Encoder}

object Codec {
  implicit def encodeMaybe[A](encodeA: Encoder[A]): Encoder[Option[A]] =
    Encoder.encodeOption(encodeA)

  implicit def decodeMaybe[A](decodeA: Decoder[A]): Decoder[Option[A]] =
    Decoder.decodeOption(decodeA)
}
