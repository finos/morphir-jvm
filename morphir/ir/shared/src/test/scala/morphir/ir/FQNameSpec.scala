package morphir.ir

import morphir.ir.codec.AllCodecs
import morphir.ir.fuzzer.AllFuzzers
import morphir.ir.testing.JsonSpec
import zio.test._
import FQName.fQName
import Path.path
import Name.name

object FQNameSpec extends DefaultRunnableSpec with JsonSpec with AllFuzzers with AllCodecs {
  def spec = suite("FQNameSpec")(
    suite("Encoding and decoding")(
      testEncodesToJSON(
        fQName(
          path(name("morphir"), name("core")),
          path(name("morphir"), name("i", "r")),
          name("f", "q", "name")
        ),
        """[[["morphir"],["core"]],[["morphir"],["i","r"]],["f","q","name"]]"""
      ),
      testM("Should encode/decode in a well-behaved manner (roundtrips).")(
        check(fuzzFQName)(fqn => checkCodecIsWellBehaved(fqn))
      )
    )
  )
}
