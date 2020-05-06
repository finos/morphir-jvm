package morphir.ir
import morphir.ir.Name.name
import morphir.ir.QName.qName
import morphir.ir.codec.AllCodecs
import morphir.ir.fuzzer.AllFuzzers
import morphir.ir.testing.JsonSpec
import zio.test._

object QNameSpec extends DefaultRunnableSpec with JsonSpec with AllCodecs with AllFuzzers {

  def spec = suite("QNameSpec")(
    suite("Encoding/Decoding a QName")(
      testEncodesToJSON(
        qName(morphirIRModulePath, name("name")),
        """[[["morphir"],["i","r"]],["name"]]"""
      ),
      testEncodesToJSON(
        qName(morphirIRModulePath, name("q", "name")),
        """[[["morphir"],["i","r"]],["q","name"]]"""
      ),
      testEncodesToJSON(
        qName(morphirIRAdvancedModulePath, name("type")),
        """[[["morphir"],["i","r"],["advanced"]],["type"]]"""
      ),
      testM("should work in a well-behaved manner")(
        check(fuzzQName)(checkCodecIsWellBehaved(_))
      )
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
