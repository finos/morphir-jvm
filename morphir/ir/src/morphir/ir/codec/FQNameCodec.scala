package morphir.ir.codec

import morphir.ir.name.Name
import morphir.ir.path.Path
import morphir.ir.FQName
import upickle.default._

trait FQNameCodec {

  implicit val readWriter: ReadWriter[FQName] =
    readwriter[(Path, Path, Name)].bimap[FQName](fqn => (fqn.packagePath, fqn.modulePath, fqn.localName), {
      case (packagePath, modulePath, localName) => FQName(packagePath, modulePath, localName)
    })
}

object FQNameCodec extends FQNameCodec
