package morphir.ir.codec

import io.circe.{ Decoder, Encoder }
import morphir.ir.{ Name, Path, QName }

trait QNameCodec {
  implicit val encodeQName: Encoder[QName] =
    Encoder.encodeTuple2[Path, Name].contramap(_.toTuple)

  implicit val decodeQName: Decoder[QName] =
    Decoder.decodeTuple2[Path, Name].map { case (modulePath, localName) => QName(modulePath, localName) }
}

object QNameCodec extends QNameCodec
