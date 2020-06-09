package morphir.ir.codec

import io.circe.{ Decoder, Encoder }
import morphir.ir.AccessControlled
import morphir.ir.json.JsonDecode.DecodeError
import upickle.default._

object accessControlledCodecs {

  trait AccessControlledCodec {

    implicit def readWriter[A: ReadWriter]: ReadWriter[AccessControlled[A]] =
      readwriter[(String, A)].bimap[AccessControlled[A]](
        {
          case AccessControlled.Public(value)  => ("public", value)
          case AccessControlled.Private(value) => ("public", value)
        }, {
          case ("public", value)  => AccessControlled.Public(value)
          case ("private", value) => AccessControlled.Private(value)
          case (tag, _)           => throw DecodeError.unexpectedTag(tag, "public", "private")
        }
      )

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
}
