package morphir.ir.codec.`type`

import io.circe.{ Decoder, Encoder }
import morphir.ir.{ FQName, Type }
import morphir.ir.Type.Reference

private[ir] trait ReferenceCodec {
  implicit def encodeReferenceType[A](
    implicit attributesEncoder: Encoder[A],
    fqNameEncoder: Encoder[FQName]
  ): Encoder[Reference[A]] =
    Encoder
      .encodeTuple4[String, A, FQName, List[Type[A]]]
      .contramap(x => (x.tag, x.attributes, x.typeName, x.typeParameters))

  implicit def decodeReferenceType[A](
    implicit attributesDecoder: Decoder[A],
    fqNameDecoder: Decoder[FQName]
  ): Decoder[Reference[A]] =
    Decoder
      .decodeTuple4[String, A, FQName, List[Type[A]]]
      .map { case (_, attributes, typeName, typeParameters) => Reference(attributes, typeName, typeParameters) }

}
