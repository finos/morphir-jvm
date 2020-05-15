package morphir.ir.codec.`type`
import io.circe.{ Decoder, Encoder }
import morphir.ir.Type
private[ir] trait ExtensibleRecordCodec {
  implicit def encodeExtensibleRecordType[A]: Encoder[Type.ExtensibleRecord[A]] = ???
  implicit def decodeExtensibleRecordType[A]: Decoder[Type.ExtensibleRecord[A]] = ???
}
