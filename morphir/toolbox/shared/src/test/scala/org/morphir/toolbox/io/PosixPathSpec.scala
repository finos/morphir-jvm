package org.morphir.toolbox.io
import zio.test._
import zio.test.Assertion._

object PosixPathSpec extends DefaultRunnableSpec {
  import Path._
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
              assert(parseRelFile("image.png"))(isSome(equalTo(file("image.png"))))
            ),
            test("preceded with current directory")(
              assert(parseRelFile("./image.png"))(isSome(equalTo(file("image.png"))))
            ),
            test("path with two segments")(
              assert(parseRelFile("foo/image.png"))(isSome(equalTo(dir("foo") </> file("image.png"))))
            ),
            test("preceded by a parent directory reference")(
              assert(parseRelFile("../foo/image.png"))(
                isSome(equalTo(currentDir <::> dir("foo") </> file("image.png")))
              )
            )
          ),
          suite("should fail parse")(
            test("with a leading /")(
              assert(parseRelFile("/foo/image.png"))(isNone)
            ),
            test("with a trailing /")(
              assert(parseRelFile("foo/"))(isNone)
            ),
            test(".")(
              assert(parseRelFile("."))(isNone)
            ),
            test("foo/..")(
              assert(parseRelFile("foo/.."))(isNone)
            )
          )
        ),
        suite("parseAbsFile")(
          suite("should successfully parse")(
            test("a simple filename with extensions as long as there is a leading /")(
              assert(parseAbsFile("/image.png"))(isSome(equalTo(rootDir </> file("image.png"))))
            ),
            test("a path with two segments")(
              assert(parseAbsFile("/foo/image.png"))(isSome(equalTo(rootDir </> dir("foo") </> file("image.png"))))
            )
          ),
          suite("fail to parse")(
            test("with a trailing /")(
              assert(parseAbsFile("/foo/"))(isNone)
            ),
            test("with no leading /")(
              assert(parseAbsFile("foo/image.png"))(isNone)
            ),
            test("/.")(
              assert(parseAbsFile("/."))(isNone)
            ),
            test("/foo/..")(
              assert(parseAbsFile("/foo/.."))(isNone)
            )
          )
        ),
        suite("parseRelDir")(
          suite("should successfully parse")(
            test("empty string")(
              assert(parseRelDir(""))(isSome(equalTo(currentDir[Unsandboxed])))
            ),
            test("./../")(
              assert(parseRelDir("./../"))(isSome(equalTo(currentDir <::> currentDir)))
            ),
            test("segment with trailing /")(
              assert(parseRelDir("foo/"))(isSome(equalTo(dir("foo"))))
            ),
            test("segment with trailing .")(
              assert(parseRelDir("foo/."))(isSome(equalTo(dir("foo") </> currentDir)))
            ),
            test("two segments with trailing /")(
              assert(parseRelDir("foo/bar/"))(isSome(equalTo(dir("foo") </> dir("bar"))))
            ),
            test("two segments starting with a reference to current directory")(
              assert(parseRelDir("./foo/bar/"))(isSome(equalTo(currentDir </> dir("foo") </> dir("bar"))))
            )
          ),
          suite("fail to parse")(
            test("leading with a /")(
              assert(parseRelDir("/foo/"))(isNone)
            ),
            test("simple name")(
              assert(parseRelDir("foo"))(isNone)
            )
          )
        ),
        suite("parseRelAsDir")(
          test("./foo/bar")(
            assert(parseRelAsDir("./foo/bar"))(isSome(equalTo(dir("foo") </> dir("bar"))))
          ),
          test("./foo/bar/")(
            assert(parseRelAsDir("./foo/bar/"))(isSome(equalTo(dir("foo") </> dir("bar"))))
          )
        ),
        suite("parseAbsDir")(
          suite("should successfully parse")(
            test("/")(
              assert(parseAbsDir("/"))(isSome(equalTo(rootDir[Unsandboxed])))
            ),
            test("/foo/")(
              assert(parseAbsDir("/foo/"))(isSome(equalTo(rootDir </> dir("foo"))))
            ),
            test("/foo/bar/")(
              assert(parseAbsDir("/foo/bar/"))(isSome(equalTo(rootDir </> dir("foo") </> dir("bar"))))
            )
          ),
          suite("should fail to parse")(
            test("/foo")(
              assert(parseAbsDir("/foo"))(isNone)
            ),
            test("foo")(
              assert(parseAbsDir("foo"))(isNone)
            )
          )
        ),
        suite("parseAbsAsDir")(
          test("/foo/bar/")(
            assert(parseAbsAsDir("/foo/bar/"))(isSome(equalTo(rootDir </> dir("foo") </> dir("bar"))))
          ),
          test("/foo/bar")(
            assert(parseAbsAsDir("/foo/bar"))(isSome(equalTo(rootDir </> dir("foo") </> dir("bar"))))
          )
        )
      )
    )
}
