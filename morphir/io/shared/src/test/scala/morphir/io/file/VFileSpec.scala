package morphir.io.file

import zio.ZIO
import zio.test._
import zio.test.Assertion._

object VFileSpec extends DefaultRunnableSpec {
  def spec = suite("VFile Spec")(
    suite("Creation")(
      suite("Creating with a string ")(
        testM("Must allow content retrieval of that string")(
          for {
            file <- VFile.make(VFilePath.of("hello.txt"), "Hello World!")
            text <- file.text
          } yield assert(text)(isSome(equalTo("Hello World!")))
        )
      ),
      suite("Creating with a text producing effect")(
        testM("Should use that effect to get text")(
          for {
            file <- VFile.makeM(VFilePath.of("README.md"), ZIO.some("Hello World"))
            text <- file.text
          } yield assert(text)(isSome(equalTo("Hello World")))
        )
      ),
      suite("From resource")(
        testM("It should be possible to read a VFile from a resource")(
          VFile.fromResource("models/hello/src/elm/Hello.elm").use { file =>
            assertM(file.text)(isSome(equalTo("""module Hello exposing (..)
                                                |
                                                |
                                                |main : String
                                                |main =
                                                |    "Hello World"
                                                |
                                                |""".stripMargin)))
          }
        ) //@@ zio.test.TestAspect.ignore
      )
    )
  )
}
