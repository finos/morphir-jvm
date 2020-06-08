package morphir

import morphir.ir.fuzzer.AllFuzzers

package object ir {

  type Name = name.Name
  val Name: name.Name.type = name.Name

  type Path = path.Path
  val Path: path.Path.type = path.Path

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

  type Argument[+A]     = (Name, A)
  type ArgumentList[+A] = List[Argument[A]]

  type RecordField[+A]  = (Name, Value[A])
  type RecordFields[+A] = List[RecordField[A]]

  type PatternMatchCases[+A] = List[PatternMatchCase[A]]

  type PatternList[+A] = List[Pattern[A]]

  type LiteralValue = Literal
  val LiteralValue: Literal.type = Literal

  type ValueExprList[+A] = List[Value[A]]

  object implicits extends AllFuzzers

}
