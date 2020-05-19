package morphir.ir.codec.`type`

import io.circe.{ Decoder, Encoder }
import morphir.ir.{ Name, Type }
import morphir.ir.Type.Field

private[ir] trait FieldCodec {
  implicit def encodeFieldType[A](
    implicit nameEncoder: Encoder[Name],
    typeEncoder: Encoder[Type[A]]
  ): Encoder[Field[A]] =
    Encoder.encodeTuple2[Name, Type[A]].contramap(ft => ft.name -> ft.fieldType)

  implicit def decodeFieldType[A](
    implicit nameDecoder: Decoder[Name],
    typeDecoder: Decoder[Type[A]]
  ): Decoder[Field[A]] =
    Decoder.decodeTuple2[Name, Type[A]].map { case (fieldName, fieldType) => Field(fieldName, fieldType) }
}
