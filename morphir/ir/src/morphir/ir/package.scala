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


package morphir

import morphir.ir.fuzzer.AllFuzzers

package object ir {

  type Name = name.Name
  val Name: name.Name.type = name.Name

  type ModuleDefinition[+A] = module.Definition[A]
  val ModuleDefinition: module.Definition.type = module.Definition

  type ModuleSpecification[+A] = module.Specification[A]
  val ModuleSpecification: module.Specification.type = module.Specification

  type TypeDefinition[+A] = Type.Definition[A]
  val TypeDefinition: Type.Definition.type = Type.Definition

  type PublicAccessControlled[+A]  = AccessControlled.Public[A]
  type PrivateAccessControlled[+A] = AccessControlled.Private[A]

  type PackageDefinition = MorphirPackage.Definition[json.Value]
  val PackageDefinition: MorphirPackage.Definition.type = MorphirPackage.Definition

  type PkgDef[+A] = MorphirPackage.Definition[A]
  val PkgDef: MorphirPackage.Definition.type = MorphirPackage.Definition

  object implicits extends AllFuzzers

}
