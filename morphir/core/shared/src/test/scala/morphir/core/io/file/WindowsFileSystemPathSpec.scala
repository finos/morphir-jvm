package morphir.core.io.file

import zio.test.Assertion.equalTo
import zio.test.{ assert, suite, test, DefaultRunnableSpec }

object WindowsFileSystemPathSpec extends DefaultRunnableSpec {
  import FileSystemPath._
  import windowsCodec._

  def spec = suite("Posix print path specs")(
    test("file in directory")(
      assert(unsafePrintPath(dir("foo") </> file("bar")))(equalTo(".\\foo\\bar"))
    ),
    test("two directories") {
      assert(unsafePrintPath(dir("foo") </> dir("bar")))(equalTo(".\\foo\\bar\\"))
    },
    test("file with two parents") {
      assert(unsafePrintPath(dir("foo") </> dir("bar") </> file("image.png")))(equalTo(".\\foo\\bar\\image.png"))
    },
    test("file without extension") {
      assert(unsafePrintPath(file("image") <:> "png"))(equalTo(".\\image.png"))
    },
    test("file with extension") {
      assert(unsafePrintPath(file("image.jpg") <:> "png"))(equalTo(".\\image.png"))
    },
    test("printPath - ./..")(
      assert(unsafePrintPath(parentDir1(currentDir)))(equalTo(".\\..\\"))
    )
  )
}
