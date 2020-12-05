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

import morphir.ir.documented.Documented
import morphir.ir.name.Name
import morphir.ir.{ AccessControlled, Type, Value }
import morphir.ir.module._
import upickle.default._
import morphir.ir.path.Path

object moduleCodecs {
  trait ModulePathCodec {

    implicit val modulePathReadWriter: ReadWriter[ModulePath] = readwriter[Path].bimap[ModulePath](
      mp => mp.toPath,
      p => ModulePath(p)
    )
  }

  object ModulePathCodec extends ModulePathCodec

  trait SpecificationCodec {
    implicit def moduleSpecReadWriter[A: ReadWriter]: ReadWriter[Specification[A]] =
      readwriter[ujson.Value].bimap[Specification[A]](
        _ => ???,
        _ => ???
      )

  }

  trait DefinitionCodec {
    implicit def moduleDefinitionReadWriter[A: ReadWriter]: ReadWriter[Definition[A]] =
      readwriter[ujson.Value].bimap[Definition[A]](
        definition =>
          ujson.Obj(
            ("types", writeJs(definition.types.toList)),
            ("values", ujson.Null)
          ),
        json => {
          val typesJson  = json("types")
          val valuesJson = json("values")

          val types = typesJson.arr.map { json =>
            val name = read[Name](json(0))
            val ac   = read[AccessControlled[Documented[Type.Definition[A]]]](json(1))
            (name, ac)
          }.toMap

          val values = valuesJson.arr.map { json =>
            val name = read[Name](json(0))
            val ac   = read[AccessControlled[Value.Definition[A]]](json(1))
            (name, ac)
          }.toMap

          assert(typesJson != ujson.Null)
          assert(valuesJson != ujson.Null)
          Definition(types = types, values = values)
        }
      )
  }
}
