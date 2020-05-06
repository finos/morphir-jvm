package morphir.ir.codec

import io.circe.{ Decoder, Encoder }
import morphir.ir.Name

trait NameCodec {
  implicit val encodeName: Encoder[Name] = Encoder.encodeList(Encoder.encodeString).contramap(name => name)
  implicit val decodeName: Decoder[Name] = Decoder.decodeList(Decoder.decodeString).map(Name.fromList)
}

object NameCodec extends NameCodec
