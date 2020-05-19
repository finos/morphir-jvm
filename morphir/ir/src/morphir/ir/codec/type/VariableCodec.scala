package morphir.ir.codec.`type`

import io.circe.{ Decoder, Encoder }
import morphir.ir.{ Name, TypeExprKind }
import morphir.ir.Type.Variable
import morphir.ir.codec.NameCodec

private[ir] trait VariableCodec extends NameCodec {
  implicit def encodeVariable[A](
    implicit attributesEncoder: Encoder[A]
  ): Encoder[Variable[A]] =
    Encoder.encodeTuple3[TypeExprKind, A, Name].contramap(exp => (exp.kind, exp.attributes, exp.name))

  implicit def decodeVariable[A](
    implicit attributesDecoder: Decoder[A]
  ): Decoder[Variable[A]] =
    Decoder.decodeTuple3[String, A, Name].map { case (_, attributes, name) => Variable(attributes, name) }
}
