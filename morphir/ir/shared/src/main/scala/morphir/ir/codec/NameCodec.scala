package morphir.ir.codec

import io.circe.{ Decoder, Encoder }
import morphir.ir.Name

trait NameCodec {

  implicit def encodeName(implicit stringEncoder: Encoder[String] = Encoder.encodeString): Encoder[Name] =
    Encoder.encodeList(stringEncoder).contramap(identity)

  implicit def decodeName(implicit stringDecoder: Decoder[String] = Decoder.decodeString): Decoder[Name] =
    Decoder.decodeList(stringDecoder).map(Name.fromList)
}

object NameCodec extends NameCodec
