package morphir.ir.codec

import io.circe.{ Decoder, Encoder }
import morphir.ir.AccessControlled

trait AccessControlledCodec {
  implicit def encodeAccessControlled[A](
    implicit itemEncoder: Encoder[A],
    tagEncoder: Encoder[String] = Encoder.encodeString
  ): Encoder[AccessControlled[A]] =
    Encoder.encodeTuple2[String, A].contramap {
      case AccessControlled.Public(value)  => "Public"  -> value
      case AccessControlled.Private(value) => "Private" -> value
    }

  implicit def decodeAccessControlled[A](
    implicit itemDecoder: Decoder[A],
    stringDecoder: Decoder[String]
  ): Decoder[AccessControlled[A]] =
    Decoder.decodeTuple2[String, A].emap {
      case ("Public", value)  => Right(AccessControlled.`public`(value))
      case ("Private", value) => Right(AccessControlled.`private`(value))
      case (ac, value)        => Left(s"Unknown access controlled type: $ac for value: $value")
    }
}
