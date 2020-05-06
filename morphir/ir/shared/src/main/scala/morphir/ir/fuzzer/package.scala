package morphir.ir

import zio.random.Random
import zio.test.Gen

package object fuzzer {

  type Fuzzer[+A] = Gen[Random with zio.test.Sized, A]
  val Fuzzer: Gen.type = zio.test.Gen

  object all extends AllFuzzers
}
