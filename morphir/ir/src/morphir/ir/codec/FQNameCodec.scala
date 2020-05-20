package morphir.ir.codec

import io.circe.{ Decoder, Encoder }
import morphir.ir.{ FQName, Name, Path }

trait FQNameCodec {
  implicit def encodeFQName(
    implicit pathEncoder: Encoder[Path] = PathCodec.encodePath,
    nameEncoder: Encoder[Name] = NameCodec.encodeName
  ): Encoder[FQName] =
    Encoder.encodeTuple3[Path, Path, Name].contramap(fqn => (fqn.packagePath, fqn.modulePath, fqn.localName))

  implicit def decodeFQName(
    implicit decodePath: Decoder[Path] = PathCodec.decodePath,
    nameDecoder: Decoder[Name] = NameCodec.decodeName
  ): Decoder[FQName] =
    Decoder.decodeTuple3[Path, Path, Name].map(FQName.fromTuple)
}

object FQNameCodec extends FQNameCodec
