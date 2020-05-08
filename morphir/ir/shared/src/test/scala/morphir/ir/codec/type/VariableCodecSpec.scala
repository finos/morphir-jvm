package morphir.ir.codec.`type`

import io.circe.Json
import morphir.ir.Name.name
import morphir.ir.Type.Variable
import morphir.ir.testing.JsonSpec
import zio.test._
import zio.test.Assertion._

object VariableCodecSpec extends DefaultRunnableSpec with JsonSpec {
  def spec = suite("(Type)Variable codec Spec")(
    suite("JSON - Encoding")(
      test("Should encode to a JSON array")(
        assert {
          import io.circe.Encoder._
          val sut = Variable(name("morphir", "test", "model", "foo"), ("one", 2))
          encodeAsJson(sut)
        }(
          equalTo(
            Json.arr(
              Json.fromString("Variable"),
              Json.arr(
                Json.fromString("one"),
                Json.fromDoubleOrNull(2)
              ),
              Json.arr(
                Json.fromString("morphir"),
                Json.fromString("test"),
                Json.fromString("model"),
                Json.fromString("foo")
              )
            )
          )
        )
      ),
      test("A type variable should encode to a JSON array where the first element is the tag name")(
        assert {
          import io.circe.Encoder._
          val sut = Variable(name("morphir", "test", "model", "foo"), ())
          encodeAsJson(sut).asArray
        }(
          isSome(hasFirst(equalTo(Json.fromString("Variable2"))))
        )
      )
    )
  )
}
