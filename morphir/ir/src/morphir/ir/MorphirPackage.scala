package morphir.ir

import io.estatico.newtype.macros.newtype
import io.circe.{ Decoder, Encoder, Json }
import io.circe.syntax._

import upickle.default._

object MorphirPackage {

  @newtype case class PackagePath(toPath: Path)
  object PackagePath {
    implicit val readWriter: ReadWriter[PackagePath] = readwriter[Path].bimap(
      pkgPath => pkgPath.toPath,
      path => PackagePath(path)
    )
  }

  sealed trait PackageRef
  object PackageRef {
    case object ThisPackage                                        extends PackageRef
    final case class PackageDependency(toPackagePath: PackagePath) extends PackageRef
  }

  case class Pkg[+A](
    dependencies: Map[PackagePath, Specification[A]],
    modules: Map[ModulePath, AccessControlled[Module.Definition[A]]]
  ) {
    def toPackageDefinition: PkgDef[A] = Definition[A](dependencies, modules)
  }

  object Pkg {}

  final case class Specification[+A](modules: Map[ModulePath, Module.Specification[A]])
  object Specification {

    implicit def readWriter[A: ReadWriter]: ReadWriter[Specification[A]] = ???

    implicit def encodeSpecification[A: Encoder]: Encoder[Specification[A]] = Encoder.instance { spec =>
      def encodeModules = spec.modules.toList.map {
        case (modulePath, moduleSpec) =>
          Json.obj(
            ("name", modulePath.asJson),
            ("spec", moduleSpec.asJson)
          )
      }

      Json.obj(
        ("modules", Json.arr(encodeModules: _*))
      )
    }

    implicit def decodeSpecifcation[A]: Decoder[Specification[A]] = Decoder.instance { _ =>
      //val modulesCursor = c.downField("modules")

      ???
    }

    def empty[A]: Specification[A] = Specification[A](Map.empty)
  }

  final case class PackageSpecEntry[+A](name: ModulePath, spec: Module.Specification[A])

  final case class Definition[+A](
    dependencies: Map[PackagePath, Specification[A]],
    modules: Map[ModulePath, AccessControlled[Module.Definition[A]]]
  )
  object Definition {

    implicit def readWriter[A: ReadWriter]: ReadWriter[Definition[A]] = {
      def writeJsonValue(defn: Definition[A]): ujson.Value =
        ujson.Obj(("dependencies", writeJs(defn.dependencies)), ("modules", ujson.Null))

      def readJsonValue(json: ujson.Value): Definition[A] = {
        val dependencies = read[List[(PackagePath, Specification[A])]](json("dependencies")).toMap
        Definition[A](dependencies, Map.empty)
      }

      readwriter[ujson.Value].bimap[Definition[A]](writeJsonValue, readJsonValue)
    }

    implicit def encodeDefinition[A: Encoder]: Encoder[Definition[A]] = ???
    implicit def decodeDefinition[A: Decoder]: Decoder[Definition[A]] = ???

    def empty[A]: Definition[A] = Definition(Map.empty, Map.empty)
  }

  @inline def emptySpecification[A]: Specification[A] = Specification.empty
  @inline def emptyDefinition[A]: Definition[A]       = Definition.empty

}
