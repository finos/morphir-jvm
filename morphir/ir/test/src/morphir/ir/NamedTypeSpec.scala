package morphir.ir

import zio.test._
import zio.test.TestAspect._
import morphir.ir.fuzzer.NameFuzzers._
import morphir.ir.testing.JsonSpec

object NamedTypeSpec extends DefaultRunnableSpec with JsonSpec {
  def spec =
    suite("NamedType Spec")(
      suite("JSON encoding")(
        testM("Ensure Codec is well behaved")(
          checkM(fuzzName) { name =>
            val sut = NamedType(name, Type.Unit("Test"))
            checkCodecIsWellBehaved(sut)
          }
        ),
        testM("Ensure Codec is well behaved for a List of NamedTypes")(
          checkM(fuzzName) { name =>
            val sut = List(NamedType(name, Type.Unit("Test")))
            checkCodecIsWellBehaved(sut)
          }
        )
      )
    ) @@ silent
}
