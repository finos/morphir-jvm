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

  object implicits extends AllCodecs with AllFuzzers with NameInstances
}
