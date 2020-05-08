package morphir.ir.codec.`type`

import io.circe.{ Decoder, Encoder }
import morphir.ir.{ FQName, Type, TypeExprKind }
import morphir.ir.Type.Reference

private[ir] trait ReferenceCodec {
  implicit def encodeReferenceType[A](
    implicit attributesEncoder: Encoder[A],
    fqNameEncoder: Encoder[FQName],
    typeExprEncoder: Encoder[TypeExprKind]
  ): Encoder[Reference[A]] =
    Encoder
      .encodeTuple4[TypeExprKind, A, FQName, List[Type[A]]]
      .contramap(x => (x.typeExprKind, x.attributes, x.typeName, x.typeParameters))

  implicit def decodeReferenceType[A](
    implicit attributesDecoder: Decoder[A],
    fqNameDecoder: Decoder[FQName]
  ): Decoder[Reference[A]] =
    Decoder
      .decodeTuple4[String, A, FQName, List[Type[A]]]
      .map { case (_, attributes, typeName, typeParameters) => Reference(typeName, typeParameters, attributes) }

}
