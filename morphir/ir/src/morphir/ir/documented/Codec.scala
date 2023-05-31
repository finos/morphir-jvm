package morphir.ir.documented

import scala.language.reflectiveCalls

/**
 * Generated based on IR.Documented
 */
object Codec {

  implicit def encodeDocumented[A](
    encodeA: io.circe.Encoder[A]
  ): io.circe.Encoder[morphir.ir.Documented.Documented[A]] =
    (
      (documented: { def doc: morphir.sdk.String.String; def value: A }) =>
        io.circe.Json.obj(
          ("doc", morphir.sdk.string.Codec.encodeString(documented.doc)),
          ("value", encodeA(documented.value))
        )
    )

  implicit def decodeDocumented[A](
    decodeA: io.circe.Decoder[A]
  ): io.circe.Decoder[morphir.ir.Documented.Documented[A]] =
    (
      (c: io.circe.HCursor) =>
        for {
          doc_   <- c.downField("doc").as(morphir.sdk.string.Codec.decodeString)
          value_ <- c.downField("value").as(decodeA)
        } yield morphir.ir.Documented.Documented(
          doc_,
          value_
        )
    )

}
