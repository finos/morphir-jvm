package morphir.ir.codec

import upickle.default._
import morphir.ir.{ module, MorphirPackage }
import morphir.ir.module.{ ModulePath, Specification => ModuleSpec }

object packageCodecs {

  trait SpecificationCodec {
    import MorphirPackage.Specification

    implicit def specificationReadWriter[A: ReadWriter]: ReadWriter[MorphirPackage.Specification[A]] = {
      def writeJsonValue(spec: Specification[A]): ujson.Value = {
        val modules = spec.modules.map {
          case (modulePath, moduleSpec) =>
            ujson.Obj(
              ("name", writeJs(modulePath)),
              ("spec", writeJs(moduleSpec))
            )
        }
        ujson.Obj(
          ("modules", ujson.Arr(modules))
        )
      }

      def readJsonValue(json: ujson.Value): Specification[A] = {
        val modulesJson = json("modules")
        val modules: Map[ModulePath, module.Specification[A]] = modulesJson.arr.map { (elem: ujson.Value) =>
          val modulePath = read[ModulePath](elem("name"))
          val moduleSpec = read[ModuleSpec[A]](elem("spec"))
          (modulePath, moduleSpec)
        }.toMap
        Specification(modules)
      }

      readwriter[ujson.Value].bimap[Specification[A]](writeJsonValue, readJsonValue)
    }
  }

  trait DefinitionCodec {
    import MorphirPackage._

    implicit def readWriter[A: ReadWriter]: ReadWriter[Definition[A]] = {
      def writeJsonValue(defn: Definition[A]): ujson.Value = {
        val dependencies = defn.dependencies.map {
          case (packageName, spec) =>
            ujson.Obj(
              ("name", writeJs(packageName)),
              ("spec", writeJs(spec))
            )
        }

        val modules = defn.modules.map {
          case (moduleName, moduleDef) =>
            ujson.Obj(
              ("name", writeJs(moduleName)),
              ("def", writeJs(moduleDef))
            )
        }

        ujson.Obj(("dependencies", ujson.Arr(dependencies)), ("modules", ujson.Arr(modules)))
      }

      def readJsonValue(json: ujson.Value): Definition[A] = {
        val dependencies = read[List[(PackagePath, Specification[A])]](json("dependencies")).toMap
        Definition[A](dependencies, Map.empty)
      }

      readwriter[ujson.Value].bimap[Definition[A]](writeJsonValue, readJsonValue)
    }
  }
}
