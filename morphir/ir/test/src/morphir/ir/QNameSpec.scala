package morphir.ir

import morphir.ir.QName.qName
import morphir.ir.fuzzer.AllFuzzers
import morphir.ir.json.JsonFacade
import morphir.ir.testing.JsonSpec
import zio.test.TestAspect._
import zio.test._

object QNameSpec extends DefaultRunnableSpec with JsonSpec with AllFuzzers with JsonFacade {

  def spec = suite("QNameSpec")(
    suite("Encoding/Decoding a QName")(
      test("Encoder test scenario 1 -")(
        assertEncodesToExpectedCompactJsonString(qName(morphirIRModulePath, name("name")))(
          """[[["morphir"],["i","r"]],["name"]]"""
        )
      ),
      test("Encoder test scenario 2 -")(
        assertEncodesToExpectedCompactJsonString(qName(morphirIRModulePath, name("q", "name")))(
          """[[["morphir"],["i","r"]],["q","name"]]"""
        )
      ),
      test("Encoder test scenario 3 - ")(
        assertEncodesToExpectedCompactJsonString(qName(morphirIRAdvancedModulePath, name("type")))(
          """[[["morphir"],["i","r"],["advanced"]],["type"]]"""
        )
      ),
      testM("should work in a well-behaved manner")(
        checkM(fuzzQName)(checkCodecIsWellBehaved(_))
      ) @@ silent
    )
  )

  val morphirIRModulePath: Path =
    Path.fromNames(
      name("morphir"),
      name("i", "r")
    )

  val morphirIRAdvancedModulePath: Path =
    Path.fromNames(
      name("morphir"),
      name("i", "r"),
      name("advanced")
    )
}
