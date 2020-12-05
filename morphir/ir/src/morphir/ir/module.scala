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

package morphir.ir

import io.estatico.newtype.macros.newtype
import morphir.ir.path.Path
import morphir.ir.codec.moduleCodecs
import morphir.ir.codec.moduleCodecs.ModulePathCodec
import morphir.ir.documented.Documented
import upickle.default._

object module {

  @newtype case class ModulePath(toPath: Path) {
    override def toString: String = s"MP: $toPath"
  }

  object ModulePath {
    def fromString(pathStr: String): ModulePath = module.ModulePath(Path.fromString(pathStr))
    def fromList(names: List[Name]): ModulePath =
      ModulePath(Path(names))

    implicit def toPath(modulePath: ModulePath): Path = modulePath.toPath

    implicit val readWriter: ReadWriter[ModulePath] = ModulePathCodec.modulePathReadWriter
  }

  final case class Specification[+A](
    types: Map[Name, Documented[Type.Specification[A]]],
    values: Map[Name, Value.Specification[A]]
  )

  object Specification extends moduleCodecs.SpecificationCodec {

    def empty[A]: Specification[A] = Specification[A](Map.empty, Map.empty)
  }
  final case class Definition[+A](
    types: Map[Name, AccessControlled[Documented[Type.Definition[A]]]],
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
