package morphir.ir.codec

import upickle.default._
import morphir.ir.Name

trait NameCodec {

  implicit def nameReadWriter: ReadWriter[Name] =
    readwriter[List[String]].bimap[Name](
      name => name.toList,
      segments => Name.fromList(segments)
    )
}

object NameCodec extends NameCodec
