package morphir.ir.codec.`type`

import io.circe.{ Decoder, Encoder }
import morphir.ir.Type

private[ir] trait TupleCodec {
  implicit def encodeTupleType[A](
    implicit attributesEncoder: Encoder[A]
  ): Encoder[Type.Tuple[A]] =
    Encoder.encodeTuple3[String, A, List[Type[A]]].contramap(tuple => (tuple.tag, tuple.attributes, tuple.elementTypes))

  implicit def decodeTupleType[A: Decoder]: Decoder[Type.Tuple[A]] =
    Decoder.decodeTuple3[String, A, List[Type[A]]].map {
      case (_, attributes, elements: List[Type[A]]) => Type.Tuple(attributes, elements)
    }
}
