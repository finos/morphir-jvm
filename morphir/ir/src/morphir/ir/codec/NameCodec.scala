package morphir.ir.codec

import io.circe.{ Decoder, Encoder }
import upickle.default._
import morphir.ir.Name

trait NameCodec {

  implicit def nameReadWriter: ReadWriter[Name] =
    readwriter[List[String]].bimap[Name](
      name => name.toList,
      segments => Name.fromList(segments)
    )

  implicit val encodeName: Encoder[Name] =
    Encoder.encodeList(Encoder.encodeString).contramap(_.value)

  implicit val decodeName: Decoder[Name] =
    Decoder.decodeList(Decoder.decodeString).map(Name.fromList)
}

object NameCodec extends NameCodec
