package morphir.ir.codec.`type`

import io.circe.{ Decoder, Encoder }
import morphir.ir.Type

private[ir] trait RecordCodec {
  implicit def encodeRecordType[A]: Encoder[Type.Record[A]] = ???
  implicit def decodeRecordType[A]: Decoder[Type.Record[A]] = ???
}
