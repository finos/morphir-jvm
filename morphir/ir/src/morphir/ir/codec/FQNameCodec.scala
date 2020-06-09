package morphir.ir.codec

import io.circe.{ Decoder, Encoder }
import morphir.ir.{ FQName, Name, Path }
import upickle.default._

trait FQNameCodec {

  implicit val readWriter: ReadWriter[FQName] =
    readwriter[(Path, Path, Name)].bimap[FQName](fqn => (fqn.packagePath, fqn.modulePath, fqn.localName), {
      case (packagePath, modulePath, localName) => FQName(packagePath, modulePath, localName)
    })

  implicit val encodeFQName: Encoder[FQName] =
    Encoder.encodeTuple3[Path, Path, Name].contramap(fqn => (fqn.packagePath, fqn.modulePath, fqn.localName))

  implicit val decodeFQName: Decoder[FQName] =
    Decoder.decodeTuple3[Path, Path, Name].map {
      case (packagePath, modulePath, localName) => FQName(packagePath, modulePath, localName)
    }
}

object FQNameCodec extends FQNameCodec
