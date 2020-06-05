package morphir.ir.codec

import morphir.ir.MorphirPackage.Pkg
import morphir.ir.json.Decode.Decoder

trait PackageCodec {
  implicit def decodePkg[A]: Decoder[Pkg[A]] = ???
}
