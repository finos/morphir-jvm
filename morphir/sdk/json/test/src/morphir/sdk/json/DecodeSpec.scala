package morphir.sdk.json

import zio.test._
import zio.test.Assertion._
import zio.test.environment._

object DecodeSpec extends DefaultRunnableSpec {
  def spec = suite("json.Decode Spec")(
    suite("Calling indent")(
      test("Should work for things with windows style line endings.") {
        val original = "Line1\r\nLine2\r\nLine3"
        assert(Decode.indent(original))(
          equalTo("Line1\r\n    Line2\r\n    Line3")
        )
      },
      test("Should work for things with linux style line endings.") {
        val original = "Line1\r\nLine2\r\nLine3"
        assert(Decode.indent(original))(
          equalTo("Line1\r\n    Line2\r\n    Line3")
        )
      }
    )
  )
}
