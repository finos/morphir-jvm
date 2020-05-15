package morphir.ir.codec

import io.circe.{ Decoder, Encoder }
import morphir.ir.{ Name, Path, QName }

trait QNameCodec {
  implicit def encodeQName(implicit pathEncoder: Encoder[Path], nameEncoder: Encoder[Name]): Encoder[QName] =
    Encoder.encodeTuple2[Path, Name].contramap(_.toTuple)

  implicit def decodeQName(implicit pathDecoder: Decoder[Path], nameDecoder: Decoder[Name]): Decoder[QName] =
    Decoder.decodeTuple2[Path, Name].map { case (modulePath, localName) => QName(modulePath, localName) }
}

object QNameCodec extends QNameCodec
