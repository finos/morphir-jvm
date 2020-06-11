package morphir.ir.codec.`type`

import morphir.ir._
import FQName.fQName
import Name.name
import morphir.ir.testing.JsonSpec
import org.scalactic.Good
import zio.test._
import zio.test.Assertion._

object ReferenceCodecSpec extends DefaultRunnableSpec with JsonSpec {
  def spec = suite("ReferenceCodec Spec")(
    suite("JSON - Decoding")(
      test("Decoding a type reference") {
        val json =
          """
            |[
            |  "reference",
            |  {},
            |  [
            |    [["morphir"], ["s", "d", "k"]],
            |    [["string"]],
            |    ["string"]
            |  ],
            |  []
            |]""".stripMargin

        val actual = decodeString[Type.Reference[Unit]](json)
        val expectedFQName = fQName(
          path(name("morphir"), name("s", "d", "k")),
          path(name("string")),
          name("string")
        )
        val expected = Type.Reference((), expectedFQName)
        assert(actual)(equalTo(Good(expected)))

      },
      test("Decoding a type reference as a Type") {
        val json =
          """
            |[
            |  "reference",
            |  {},
            |  [
            |    [["morphir"], ["s", "d", "k"]],
            |    [["string"]],
            |    ["string"]
            |  ],
            |  []
            |]""".stripMargin

        val actual = decodeString[Type[Unit]](json)
        val expectedFQName = fQName(
          path(name("morphir"), name("s", "d", "k")),
          path(name("string")),
          name("string")
        )
        val expected = Type.Reference((), expectedFQName)
        assert(actual)(equalTo(Good(expected)))

      },
      test("Decoding a type reference should fail if the tag is wrong") {
        val json =
          """
            |[
            |  "not-a-reference",
            |  {},
            |  [
            |    [["morphir"], ["s", "d", "k"]],
            |    [["string"]],
            |    ["string"]
            |  ],
            |  []
            |]""".stripMargin

        val actual = decodeString[Type.Reference[Unit]](json)
        val expectedFQName = fQName(
          path(name("morphir"), name("s", "d", "k")),
          path(name("string")),
          name("string")
        )
        val expected = Type.Reference((), expectedFQName)
        assert(actual)(not(equalTo(Good(expected))))

      }
    )
  )
}
