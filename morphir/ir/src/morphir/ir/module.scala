package morphir.ir

import io.circe.{ Decoder, Encoder, Json }
import io.circe.syntax._
import io.estatico.newtype.macros.newtype
import morphir.ir.codec.moduleCodecs
import morphir.ir.codec.moduleCodecs.ModulePathCodec
import upickle.default._

object module {

  @newtype case class ModulePath(toPath: Path)

  object ModulePath {
    def fromString(pathStr: String): ModulePath = module.ModulePath(Path.fromString(pathStr))
    def fromList(names: List[Name]): ModulePath =
      ModulePath(Path(names))

    implicit def toPath(modulePath: ModulePath): Path = modulePath.toPath

    implicit val readWriter: ReadWriter[ModulePath] = ModulePathCodec.modulePathReadWriter

    implicit val encodeModulePath: Encoder[ModulePath] =
      ModulePathCodec.encodeModulePath

    implicit val decodeModulePath: Decoder[ModulePath] =
      ModulePathCodec.decodeModulePath
  }

  final case class Specification[+A](
    types: Map[Name, Type.Specification[A]],
    values: Map[Name, Value.Specification[A]]
  )

  object Specification extends moduleCodecs.SpecificationCodec {

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

  object Definition extends moduleCodecs.DefinitionCodec {
    def empty[A]: Definition[A] = Definition[A](Map.empty, Map.empty)
  }

  final case class ModuleInfo[+A](path: ModulePath, definition: ModuleDefinition[A]) {
    def toTuple: (ModulePath, ModuleDefinition[A]) = path -> definition
  }
  object ModuleInfo {

    def apply[A](args: (ModulePath, ModuleDefinition[A])): ModuleInfo[A] =
      ModuleInfo(args._1, args._2)
  }

}
