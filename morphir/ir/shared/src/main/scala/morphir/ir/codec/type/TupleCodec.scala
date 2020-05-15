package morphir.ir.codec.`type`

import io.circe.{ Decoder, Encoder }
import morphir.ir.Type

private[ir] trait TupleCodec {
  implicit def encodeTupleType[A]: Encoder[Type.Tuple[A]] = ???
  implicit def decodeTupleType[A]: Decoder[Type.Tuple[A]] = ???
}
