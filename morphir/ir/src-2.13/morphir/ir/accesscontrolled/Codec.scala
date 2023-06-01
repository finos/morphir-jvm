package morphir.ir.accesscontrolled

import scala.language.reflectiveCalls

/**
 * Generated based on IR.AccessControlled
 */
object Codec {

  implicit val encodeAccess: io.circe.Encoder[morphir.ir.AccessControlled.Access] = (
    (access: morphir.ir.AccessControlled.Access) =>
      access match {
        case morphir.ir.AccessControlled.Private =>
          io.circe.Json.fromString("Private")
        case morphir.ir.AccessControlled.Public =>
          io.circe.Json.fromString("Public")
      }
  )

  implicit def encodeAccessControlled[A](
    encodeA: io.circe.Encoder[A]
  ): io.circe.Encoder[morphir.ir.AccessControlled.AccessControlled[A]] =
    (
      (accessControlled: { def access: morphir.ir.AccessControlled.Access; def value: A }) =>
        io.circe.Json.obj(
          ("access", morphir.ir.accesscontrolled.Codec.encodeAccess(accessControlled.access)),
          ("value", encodeA(accessControlled.value))
        )
    )

  implicit val decodeAccess: io.circe.Decoder[morphir.ir.AccessControlled.Access] = (
    (c: io.circe.HCursor) =>
      c.withFocus(_.withString(((str) => io.circe.Json.arr(io.circe.Json.fromString(str)))))
        .downN(0)
        .as(morphir.sdk.string.Codec.decodeString)
        .flatMap(
          (
            (tag) =>
              tag match {
                case "Private" =>
                  scala.Right(morphir.ir.AccessControlled.Private)
                case "Public" =>
                  scala.Right(morphir.ir.AccessControlled.Public)
              }
          )
        )
  )

  implicit def decodeAccessControlled[A](
    decodeA: io.circe.Decoder[A]
  ): io.circe.Decoder[morphir.ir.AccessControlled.AccessControlled[A]] =
    (
      (c: io.circe.HCursor) =>
        for {
          access_ <- c.downField("access").as(morphir.ir.accesscontrolled.Codec.decodeAccess)
          value_  <- c.downField("value").as(decodeA)
        } yield morphir.ir.AccessControlled.AccessControlled(
          access_,
          value_
        )
    )

}
