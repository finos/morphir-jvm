package morphir

import morphir.ir.codec.AllCodecs
import morphir.ir.fuzzer.AllFuzzers

package object ir {
  type Name = Name.WrappedType
  object implicits extends AllCodecs with AllFuzzers
}
