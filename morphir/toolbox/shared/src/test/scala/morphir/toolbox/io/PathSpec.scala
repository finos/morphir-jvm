package morphir.toolbox.io

import io.circe.Json
import io.circe.syntax._
import io.circe.parser._
import zio.test.Assertion.{ equalTo, isRight }
import zio.test.{ assert, suite, test, DefaultRunnableSpec }

object PathSpec extends DefaultRunnableSpec {
  import Path._
  def spec = suite("Path Spec")(
    suite("FileName Spec")(
      suite("Changing extension")(
        test("Directly to .txt")(
          assert(FileName("README.md").changeExtension("txt"))(equalTo(FileName("README.txt")))
        ),
        test("Based on current")(
          assert(FileName("Notes.txt").changeExtension(x => x + ".swp"))(equalTo(FileName("Notes.txt.swp")))
        )
      ),
      suite("JSON encoding")(
        test("Should encode as a JSON String") {
          val sut = Path.FileName("morphir.yml")
          assert(sut.asJson)(equalTo(Json.fromString("morphir.yml")))
        }
      ),
      suite("JSON decoding")(
        test("Should decode from a JSON String") {
          val input = "\"morphir.json\""
          assert(decode[Path.FileName](input))(isRight(equalTo(Path.FileName("morphir.json"))))
        }
      )
    ),
    suite("DirName Spec")(
      suite("JSON encoding")(
        test("Should encode as a JSON String") {
          val sut = Path.DirName("alpha/beta")
          assert(sut.asJson)(equalTo(Json.fromString("alpha/beta")))
        }
      ),
      suite("JSON decoding")(
        test("Should decode from a JSON String") {
          val input = "\".morphir/work\""
          assert(decode[Path.DirName](input))(isRight(equalTo(Path.DirName(".morphir/work"))))
        }
      )
    ),
    suite("Path")(
      suite("JSON encoding")(
        test("Should encode as a JSON String") {
          import Path._
          import posixCodec._
          val path = currentDir </> dir(".morphir") </> file("projects.json")
          assert(path.asJson)(equalTo(Json.fromString("./.morphir/projects.json")))
        }
      )
    )
  )
}
