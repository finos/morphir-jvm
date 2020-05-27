package morphir

import morphir.ir.codec.AllCodecs
import morphir.ir.fuzzer.AllFuzzers
import morphir.ir.typeclass.instances.NameInstances

package object ir {

  type Path = path.Path
  val Path: path.Path.type = path.Path

  val ModulePath: Module.ModulePath.type = Module.ModulePath
  type ModulePath = Module.ModulePath

  type ModuleDefinition[+A] = Module.Definition[A]
  val ModuleDefinition: Module.Definition.type = Module.Definition

  type ModuleSpecification[+A] = Module.Specification[A]
  val ModuleSpecification: Module.Specification.type = Module.Specification

  type TypeDefinition[+A] = Type.Definition[A]
  val TypeDefinition: Type.Definition.type = Type.Definition

  type PublicAccessControlled[+A]  = AccessControlled.Public[A]
  type PrivateAccessControlled[+A] = AccessControlled.Private[A]

  type Package = PackageModule.type
  val Package: Package = PackageModule

  type PkgDef[+A] = Package.Definition[A]
  val PkgDef: Package.Definition.type = Package.Definition

  type Parameter[+A]     = (Name, Type[A])
  type ParameterList[+A] = List[Parameter[A]]

  type Argument[+A]     = (Name, A)
  type ArgumentList[+A] = List[Argument[A]]

  type RecordField[+A]  = (Name, Value[A])
  type RecordFields[+A] = List[RecordField[A]]

  type PatternMatchCase[+A]  = (Pattern[A], Value[A])
  type PatternMatchCases[+A] = List[PatternMatchCase[A]]

  type PatternList[+A] = List[Pattern[A]]

  type LiteralValue = Literal
  val LiteralValue: Literal.type = Literal

  type ValueExprList[+A] = List[Value[A]]

  object implicits extends AllCodecs with AllFuzzers with NameInstances
}
