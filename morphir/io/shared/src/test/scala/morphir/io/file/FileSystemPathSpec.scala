package morphir.io.file

import io.circe.Json
import io.circe.parser._
import io.circe.syntax._
import zio.test.Assertion.{ equalTo, isRight }
import zio.test.{ assert, suite, test, DefaultRunnableSpec }

object FileSystemPathSpec extends DefaultRunnableSpec {
  import FileSystemPath._
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
          val sut = FileSystemPath.FileName("morphir.yml")
          assert(sut.asJson)(equalTo(Json.fromString("morphir.yml")))
        }
      ),
      suite("JSON decoding")(
        test("Should decode from a JSON String") {
          val input = "\"morphir.json\""
          assert(decode[FileSystemPath.FileName](input))(isRight(equalTo(FileSystemPath.FileName("morphir.json"))))
        }
      )
    ),
    suite("DirName Spec")(
      suite("JSON encoding")(
        test("Should encode as a JSON String") {
          val sut = FileSystemPath.DirName("alpha/beta")
          assert(sut.asJson)(equalTo(Json.fromString("alpha/beta")))
        }
      ),
      suite("JSON decoding")(
        test("Should decode from a JSON String") {
          val input = "\".morphir/work\""
          assert(decode[FileSystemPath.DirName](input))(isRight(equalTo(FileSystemPath.DirName(".morphir/work"))))
        }
      )
    ),
    suite("Path")(
      suite("JSON encoding")(
        test("Should encode as a JSON String") {
          import FileSystemPath._
          import posixCodec._
          val path = currentDir </> dir(".morphir") </> file("projects.json")
          assert(path.asJson)(equalTo(Json.fromString("./.morphir/projects.json")))
        }
      )
    )
  )
}
