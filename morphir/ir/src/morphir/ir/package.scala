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

  type PackageDefinition = MorphirPackage.Definition[ujson.Value]
  val PackageDefinition: MorphirPackage.Definition.type = MorphirPackage.Definition

  type PkgDef[+A] = MorphirPackage.Definition[A]
  val PkgDef: MorphirPackage.Definition.type = MorphirPackage.Definition

  object implicits extends AllFuzzers

}
