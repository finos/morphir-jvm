package morphir

import morphir.ir.codec.AllCodecs
import morphir.ir.fuzzer.AllFuzzers
import morphir.ir.typeclass.instances.NameInstances

package object ir {

  val ModulePath: Module.ModulePath.type = Module.ModulePath
  type ModulePath                  = Module.ModulePath
  type Name                        = Name.WrappedType
  type PublicAccessControlled[+A]  = AccessControlled.Public[A]
  type PrivateAccessControlled[+A] = AccessControlled.Private[A]

  type Parameter[+A]     = (Name, Type[A])
  type ParameterList[+A] = List[Parameter[A]]

  type Argument[+A]     = (Name, A)
  type ArgumentList[+A] = List[Argument[A]]

  type RecordField[+A]  = (Name, Value[A])
  type RecordFields[+A] = List[RecordField[A]]

  type PatternMatchCase[+A]  = (Value.Pattern[A], Value[A])
  type PatternMatchCases[+A] = List[PatternMatchCase[A]]

  type LiteralValue[+A] = Literal[A]

  object implicits extends AllCodecs with AllFuzzers with NameInstances
}
