package morphir

import morphir.ir.codec.AllCodecs
import morphir.ir.fuzzer.AllFuzzers
import morphir.ir.typeclass.instances.NameInstances

package object ir {
  type Name = Name.WrappedType
  object implicits extends AllCodecs with AllFuzzers with NameInstances
}
