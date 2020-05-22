package morphir.ir

import io.circe.{ Decoder, Encoder }
import io.estatico.newtype.macros.newtype
import morphir.ir.codec.NameCodec

object Module {

  @newtype case class ModulePath(toPath: Path)

  object ModulePath {
    def fromString(pathStr: String): ModulePath = Module.ModulePath(Path.fromString(pathStr))
    def fromList(names: List[Name]): ModulePath =
      ModulePath(Path(names))

    implicit def toPath(modulePath: ModulePath): Path = modulePath.toPath
    implicit def encodePath(implicit nameEncoder: Encoder[Name] = NameCodec.encodeName): Encoder[ModulePath] =
      Encoder.encodeList(nameEncoder).contramap(x => x.toPath.value)

    implicit def decodePath(implicit nameDecoder: Decoder[Name] = NameCodec.decodeName): Decoder[ModulePath] =
      Decoder.decodeList(nameDecoder).map(ModulePath.fromList)
  }

  final case class Specification[+A](
    types: Map[Name, Type.Specification[A]],
    values: Map[Name, Value.Specification[A]]
  )

  object Specification {
    def empty[A]: Specification[A] = Specification[A](Map.empty, Map.empty)
  }

  final case class Definition[+A](
    types: Map[Name, AccessControlled[Type.Definition[A]]],
    values: Map[Name, AccessControlled[Value.Definition[A]]]
  ) {}

  object Definition {
    def empty[A]: Definition[A] = Definition[A](Map.empty, Map.empty)
  }
}
