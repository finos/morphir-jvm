package morphir.ir.codec

import morphir.ir.Name
import morphir.ir.path.Path
import upickle.default._

trait PathCodec {
  import io.circe.{ Decoder, Encoder }

  implicit val pathReadWriter: ReadWriter[Path] = readwriter[List[Name]].bimap(
    path => path.toList,
    items => Path.fromList(items)
  )

  implicit val encodePath: Encoder[Path] =
    Encoder.encodeList(Name.encodeName).contramap(x => x.value)

  implicit val decodePath: Decoder[Path] =
    Decoder.decodeList(Name.decodeName).map(Path.fromList)
}

object PathCodec extends PathCodec
