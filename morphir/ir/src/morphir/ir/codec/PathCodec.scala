package morphir.ir.codec

import morphir.ir.Name
import morphir.ir.path.Path
import upickle.default._

trait PathCodec {
  implicit val pathReadWriter: ReadWriter[Path] = readwriter[List[Name]].bimap(
    path => path.toList,
    items => Path.fromList(items)
  )
}

object PathCodec extends PathCodec
