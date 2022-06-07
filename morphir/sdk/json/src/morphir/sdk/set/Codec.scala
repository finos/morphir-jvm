package morphir.sdk.set

import io.circe.{ Decoder, Encoder }
import morphir.sdk.Set.Set

object Codec {

  implicit def encodeSet[A](implicit encodeA: Encoder[A]): Encoder[Set[A]] =
    Encoder.encodeSet(encodeA)

  implicit def decodeSet[A](implicit decodeA: Decoder[A]): Decoder[Set[A]] =
    Decoder.decodeSet(decodeA)

}
