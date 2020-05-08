package morphir.ir.codec.`type`
import io.circe.{ Decoder, Encoder }
import morphir.ir.{ Type, TypeExprKind }

private[ir] trait FunctionCodec {
  implicit def encodeFunctionType[A](
    implicit teKindEncoder: Encoder[TypeExprKind],
    attributesEncoder: Encoder[A]
  ): Encoder[Type.Function[A]] =
    Encoder
      .encodeTuple4[TypeExprKind, A, Type[A], Type[A]]
      .contramap(ft => (ft.kind, ft.attributes, ft.argumentType, ft.returnType))

  implicit def decodeFunctionType[A](
    implicit teKindDecoder: Decoder[TypeExprKind],
    attributesEncoder: Decoder[A]
  ): Decoder[Type.Function[A]] =
    Decoder
      .decodeTuple4[TypeExprKind, A, Type[A], Type[A]]
      .map {
        case (_, attributes, argumentType, returnType) => Type.Function(attributes, argumentType, returnType)
      }
}
