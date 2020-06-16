package morphir.ir.codec

import morphir.ir.name.Name
import morphir.ir.path.Path
import morphir.ir.QName
import upickle.default._

trait QNameCodec {
  implicit val readWriter: ReadWriter[QName] = readwriter[(Path, Name)].bimap[QName](
    qName => (qName.modulePath, qName.localName), {
      case (modulePath, localName) => QName(modulePath, localName)
    }
  )
}

object QNameCodec extends QNameCodec
