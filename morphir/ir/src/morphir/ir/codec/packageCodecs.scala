/*
Copyright 2020 Morgan Stanley

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 */

package morphir.ir.codec

import upickle.default._
import morphir.ir.{ module, AccessControlled, MorphirPackage }
import morphir.ir.module.{ ModulePath, Specification => ModuleSpec }

object packageCodecs {

  trait SpecificationCodec {
    import MorphirPackage.Specification

    implicit def specificationReadWriter[A: ReadWriter]: ReadWriter[MorphirPackage.Specification[A]] = {
      def writeJsonValue(spec: Specification[A]): ujson.Value = {
        val modules = spec.modules.map { case (modulePath, moduleSpec) =>
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
        val modulesJson                                       = json("modules")
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
        val dependencies = defn.dependencies.map { case (packageName, spec) =>
          ujson.Obj(
            ("name", writeJs(packageName)),
            ("spec", writeJs(spec))
          )
        }

        val modules = defn.modules.map { case (moduleName, moduleDef) =>
          ujson.Obj(
            ("name", writeJs(moduleName)),
            ("def", writeJs(moduleDef))
          )
        }

        ujson.Obj(("dependencies", ujson.Arr(dependencies)), ("modules", ujson.Arr(modules)))
      }

      def readJsonValue(json: ujson.Value): Definition[A] = {
        val dependenciesJson = json("dependencies")
        val moduleJson       = json("modules")

        val dependencies = read[List[(PackagePath, Specification[A])]](dependenciesJson).toMap
        val modules      = moduleJson.arr.map { json =>
          val name = read[ModulePath](json("name"))
          val defn = read[AccessControlled[module.Definition[A]]](json("def"))
          (name, defn)
        }.toMap

        Definition[A](dependencies, modules)
      }

      readwriter[ujson.Value].bimap[Definition[A]](writeJsonValue, readJsonValue)
    }
  }
}
