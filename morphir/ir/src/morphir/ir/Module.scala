package morphir.ir

import io.circe.{ Decoder, Encoder, Json }
import io.circe.syntax._
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
    implicit def encodeSpecification[A: Encoder]: Encoder[Specification[A]] = {
      implicit def encodeTypeSpecMap: Encoder[Map[Name, Type.Specification[A]]] =
        Encoder.encodeList[(Name, Type.Specification[A])].contramap(specMap => specMap.toList)

      implicit def encodeValueSpecMap: Encoder[Map[Name, Value.Specification[A]]] =
        Encoder.encodeList[(Name, Value.Specification[A])].contramap(_.toList)

      Encoder.encodeJson.contramap { spec =>
        Json.obj(
          ("type", spec.types.asJson),
          ("values", spec.values.asJson)
        )
      }
    }

    implicit def decodeSpecification[A: Decoder]: Decoder[Specification[A]] = ???

    def empty[A]: Specification[A] = Specification[A](Map.empty, Map.empty)
  }

  final case class NamedModuleSpec[+A](name: ModulePath, spec: Specification[A])
  object NamedModuleSpec {
    implicit def encodeNamedModuleSpec[A: Encoder]: Encoder[NamedModuleSpec[A]] =
      Encoder.instance(me =>
        Json.obj(
          ("name", me.name.asJson),
          ("spec", me.spec.asJson)
        )
      )
  }

  final case class Definition[+A](
    types: Map[Name, AccessControlled[Type.Definition[A]]],
    values: Map[Name, AccessControlled[Value.Definition[A]]]
  )

  object Definition {
    implicit def encodeDefinition[A: Encoder]: Encoder[Definition[A]] = ???
    implicit def decodeDefinition[A: Decoder]: Decoder[Definition[A]] = ???

    def empty[A]: Definition[A] = Definition[A](Map.empty, Map.empty)
  }
}
