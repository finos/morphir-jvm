package morphir.ir

import io.circe.{ Decoder, Encoder }
import morphir.ir.AccessControlled.{ Private, Public }
import morphir.ir.codec.accessControlledCodecs

sealed abstract class AccessControlled[+A] extends Product with Serializable {

  def value: A

  def withPublicAccess: Option[A] = this match {
    case AccessControlled.Public(v) => Some(v)
    case _                          => None
  }

  def withPrivateAccess: A = this match {
    case AccessControlled.Public(v)  => v
    case AccessControlled.Private(v) => v
  }

  def map[B](fn: A => B): AccessControlled[B] =
    this match {
      case Public(value)  => Public(fn(value))
      case Private(value) => Private(fn(value))
    }

  def flatMap[B](fn: A => AccessControlled[B]): AccessControlled[B] = this match {
    case Public(value)  => fn(value)
    case Private(value) => fn(value)
  }

  def fold[B](whenPublic: A => B, whenPrivate: A => B): B = this match {
    case Public(value)  => whenPublic(value)
    case Private(value) => whenPrivate(value)
  }
}

object AccessControlled extends accessControlledCodecs.AccessControlledCodec {

  sealed abstract class Access extends Serializable
  object Access {
    case object Public  extends Access
    case object Private extends Access
  }

  final case class Public[+A] private[AccessControlled] (value: A) extends AccessControlled[A]

  object Public {
    implicit def encodeAccessControlled[A](
      implicit itemEncoder: Encoder[A],
      tagEncoder: Encoder[String] = Encoder.encodeString
    ): Encoder[Public[A]] =
      Encoder.encodeTuple2[String, A].contramap(ac => "Public" -> ac.value)

    implicit def decodeAccessControlled[A](
      implicit itemDecoder: Decoder[A],
      stringDecoder: Decoder[String]
    ): Decoder[Public[A]] =
      Decoder.decodeTuple2[String, A].emap {
        case ("Public", value) => Right(Public(value))
        case ("Private", value) =>
          Left(
            s"An access type of private is not a valid access controlled type for the public access controlled object: $value."
          )
        case (ac, value) => Left(s"Unknown access controlled type: $ac for value: $value")
      }
  }

  final case class Private[+A] private[AccessControlled] (value: A) extends AccessControlled[A] {}

  object Private {
    implicit def encodeAccessControlled[A](
      implicit itemEncoder: Encoder[A],
      tagEncoder: Encoder[String] = Encoder.encodeString
    ): Encoder[Private[A]] =
      Encoder.encodeTuple2[String, A].contramap(ac => "Private" -> ac.value)

    implicit def decodeAccessControlled[A](
      implicit itemDecoder: Decoder[A],
      stringDecoder: Decoder[String]
    ): Decoder[Private[A]] =
      Decoder.decodeTuple2[String, A].emap {
        case ("private", value) => Right(Private(value))
        case ("public", value) =>
          Left(
            s"An access type of public is not a valid access controlled type for the public access controlled object: $value."
          )
        case (ac, value) => Left(s"Unknown access controlled type: $ac for value: $value")
      }
  }

  def publicAccess[A](value: A): AccessControlled[A] =
    Public(value)

  def privateAccess[A](value: A): AccessControlled[A] =
    Private(value)

  def `public`[A](value: A): AccessControlled[A] =
    Public(value)

  def `private`[A](value: A): AccessControlled[A] =
    Private(value)

  @inline def withPublicAccess[A](ac: AccessControlled[A]): Option[A] =
    ac.withPublicAccess

  @inline def withPrivateAccess[A](ac: AccessControlled[A]): A =
    ac.withPrivateAccess
}
