package morphir.io.file

import zio.test.Assertion.{ equalTo, isNone, isSome }
import zio.test.{ assert, suite, test, DefaultRunnableSpec }

object PosixFileSystemPathSpec extends DefaultRunnableSpec {
  import FileSystemPath._
  import posixCodec._

  def spec =
    suite("Path Spec")(
      suite("Posix print path specs")(
        test("file in directory")(
          assert(unsafePrintPath(dir("foo") </> file("bar")))(equalTo("./foo/bar"))
        ),
        test("two directories") {
          assert(unsafePrintPath(dir("foo") </> dir("bar")))(equalTo("./foo/bar/"))
        },
        test("file with two parents") {
          assert(unsafePrintPath(dir("foo") </> dir("bar") </> file("image.png")))(equalTo("./foo/bar/image.png"))
        },
        test("file without extension") {
          assert(unsafePrintPath(file("image") <:> "png"))(equalTo("./image.png"))
        },
        test("file with extension") {
          assert(unsafePrintPath(file("image.jpg") <:> "png"))(equalTo("./image.png"))
        },
        test("printPath - ./..")(
          assert(unsafePrintPath(parentDir1(currentDir)))(equalTo("./../"))
        )
      ),
      suite("Posix path parsing specs")(
        suite("parseRelFile")(
          suite("should successfully parse")(
            test("simple file name with extension")(
              assert(parseRelativeFile("image.png"))(isSome(equalTo(file("image.png"))))
            ),
            test("preceded with current directory")(
              assert(parseRelativeFile("./image.png"))(isSome(equalTo(file("image.png"))))
            ),
            test("path with two segments")(
              assert(parseRelativeFile("foo/image.png"))(isSome(equalTo(dir("foo") </> file("image.png"))))
            ),
            test("preceded by a parent directory reference")(
              assert(parseRelativeFile("../foo/image.png"))(
                isSome(equalTo(currentDir <::> dir("foo") </> file("image.png")))
              )
            )
          ),
          suite("should fail parse")(
            test("with a leading /")(
              assert(parseRelativeFile("/foo/image.png"))(isNone)
            ),
            test("with a trailing /")(
              assert(parseRelativeFile("foo/"))(isNone)
            ),
            test(".")(
              assert(parseRelativeFile("."))(isNone)
            ),
            test("foo/..")(
              assert(parseRelativeFile("foo/.."))(isNone)
            )
          )
        ),
        suite("parseAbsFile")(
          suite("should successfully parse")(
            test("a simple filename with extensions as long as there is a leading /")(
              assert(parseAbsoluteFile("/image.png"))(isSome(equalTo(rootDir </> file("image.png"))))
            ),
            test("a path with two segments")(
              assert(parseAbsoluteFile("/foo/image.png"))(isSome(equalTo(rootDir </> dir("foo") </> file("image.png"))))
            )
          ),
          suite("fail to parse")(
            test("with a trailing /")(
              assert(parseAbsoluteFile("/foo/"))(isNone)
            ),
            test("with no leading /")(
              assert(parseAbsoluteFile("foo/image.png"))(isNone)
            ),
            test("/.")(
              assert(parseAbsoluteFile("/."))(isNone)
            ),
            test("/foo/..")(
              assert(parseAbsoluteFile("/foo/.."))(isNone)
            )
          )
        ),
        suite("parseRelDir")(
          suite("should successfully parse")(
            test("empty string")(
              assert(parseRelativeDir(""))(isSome(equalTo(currentDir[Unsandboxed])))
            ),
            test("./../")(
              assert(parseRelativeDir("./../"))(isSome(equalTo(currentDir <::> currentDir)))
            ),
            test("segment with trailing /")(
              assert(parseRelativeDir("foo/"))(isSome(equalTo(dir("foo"))))
            ),
            test("segment with trailing .")(
              assert(parseRelativeDir("foo/."))(isSome(equalTo(dir("foo") </> currentDir)))
            ),
            test("two segments with trailing /")(
              assert(parseRelativeDir("foo/bar/"))(isSome(equalTo(dir("foo") </> dir("bar"))))
            ),
            test("two segments starting with a reference to current directory")(
              assert(parseRelativeDir("./foo/bar/"))(isSome(equalTo(currentDir </> dir("foo") </> dir("bar"))))
            )
          ),
          suite("fail to parse")(
            test("leading with a /")(
              assert(parseRelativeDir("/foo/"))(isNone)
            ),
            test("simple name")(
              assert(parseRelativeDir("foo"))(isNone)
            )
          )
        ),
        suite("parseRelAsDir")(
          test("./foo/bar")(
            assert(parseRelativeAsDir("./foo/bar"))(isSome(equalTo(dir("foo") </> dir("bar"))))
          ),
          test("./foo/bar/")(
            assert(parseRelativeAsDir("./foo/bar/"))(isSome(equalTo(dir("foo") </> dir("bar"))))
          )
        ),
        suite("parseAbsDir")(
          suite("should successfully parse")(
            test("/")(
              assert(parseAbsoluteDir("/"))(isSome(equalTo(rootDir[Unsandboxed])))
            ),
            test("/foo/")(
              assert(parseAbsoluteDir("/foo/"))(isSome(equalTo(rootDir </> dir("foo"))))
            ),
            test("/foo/bar/")(
              assert(parseAbsoluteDir("/foo/bar/"))(isSome(equalTo(rootDir </> dir("foo") </> dir("bar"))))
            )
          ),
          suite("should fail to parse")(
            test("/foo")(
              assert(parseAbsoluteDir("/foo"))(isNone)
            ),
            test("foo")(
              assert(parseAbsoluteDir("foo"))(isNone)
            )
          )
        ),
        suite("parseAbsAsDir")(
          test("/foo/bar/")(
            assert(parseAbsoluteAsDir("/foo/bar/"))(isSome(equalTo(rootDir </> dir("foo") </> dir("bar"))))
          ),
          test("/foo/bar")(
            assert(parseAbsoluteAsDir("/foo/bar"))(isSome(equalTo(rootDir </> dir("foo") </> dir("bar"))))
          )
        )
      )
    )
}
