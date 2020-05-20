package morphir.ir.codec

import io.circe.{ Decoder, Encoder }
import morphir.ir.{ Name, Path }

trait PathCodec {
  implicit def encodePath(implicit nameEncoder: Encoder[Name] = NameCodec.encodeName): Encoder[Path] =
    Encoder.encodeList(nameEncoder).contramap(x => x.value)

  implicit def decodePath(implicit nameDecoder: Decoder[Name] = NameCodec.decodeName): Decoder[Path] =
    Decoder.decodeList(nameDecoder).map(Path.fromList)
}

object PathCodec extends PathCodec
