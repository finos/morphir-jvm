package morphir.ir

import morphir.ir.codec.AllCodecs
import morphir.ir.fuzzer.AllFuzzers
import morphir.ir.testing.JsonSpec
import zio.test._
import zio.test.Assertion._
import FQName.fQName
import Name.name
import morphir.ir.json.JsonFacade

object FQNameSpec extends DefaultRunnableSpec with JsonSpec with AllFuzzers with AllCodecs with JsonFacade {
  def spec = suite("FQNameSpec")(
    suite("Encoding and decoding")(
      test("Should encode properly - 1")(
        assert(
          compactEncode(
            fQName(
              path(name("morphir"), name("core")),
              path(name("morphir"), name("i", "r")),
              name("f", "q", "name")
            )
          )
        )(
          equalTo("""[[["morphir"],["core"]],[["morphir"],["i","r"]],["f","q","name"]]""")
        )
      ),
      testM("Should encode/decode in a well-behaved manner (roundtrips).")(
        check(fuzzFQName)(fqn => checkCodecIsWellBehaved(fqn))
      )
    )
  )
}
