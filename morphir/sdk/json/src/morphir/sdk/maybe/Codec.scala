package morphir.sdk.maybe

import io.circe.{Decoder, Encoder, HCursor, Json}
import morphir.sdk.Maybe
import morphir.sdk.Maybe.Maybe

object Codec {
  implicit def encodeMaybe[A](implicit encodeA: Encoder[A]): Encoder[Maybe[A]] =
    (maybeVal : Maybe[A]) =>
      maybeVal match {
        case Maybe.Just(value) => encodeA(value)
        case Maybe.Nothing => Json.Null
      }

  implicit def decodeMaybe[A](implicit decodeA: Decoder[A]): Decoder[Maybe[A]] =
    (c: HCursor) =>
      c.downN(0).as(decodeA).map{
        value =>
          value match {
            case Json.Null => Maybe.Nothing
            case va => Maybe.Just(va)
          }
      }
}





