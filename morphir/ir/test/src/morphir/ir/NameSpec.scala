package morphir.ir

import io.circe.{ Json, ParsingFailure, parser }
import morphir.ir.Name.Name
import morphir.ir.name.Codec
import zio.test.Assertion._
import zio.test._

object NameSpec extends DefaultRunnableSpec {
  val irName: Name                                 = Name.fromString("fooBar_baz 123")
  val nameInIRJson                                 = """["foo", "bar","baz", "123"]"""
  val nameFromIRJson: Either[ParsingFailure, Json] = parser.parse(nameInIRJson)

  override def spec: ZSpec[_root_.zio.test.environment.TestEnvironment, Any] = suite("Name Encoding / Decoding")(
    test("Encoding Name") {
      val encodedName          = Codec.encodeName(irName)
      val expectedNameInIRJson = nameFromIRJson.getOrElse(Json.Null)

      assert(encodedName)(equalTo(expectedNameInIRJson))
    },
    test("Decoding Name") {
      val nameFromJson = nameFromIRJson.getOrElse(Json.Null)
      val decodedName  = Codec.decodeName(nameFromJson.hcursor)

      assert(decodedName.getOrElse(Json.Null))(equalTo(irName))
    }
  )
}
