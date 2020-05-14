package morphir.io.file

import zio._
import zio.test.Assertion.{ equalTo, isSome }
import zio.test.{ assert, suite, testM, DefaultRunnableSpec }

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
        )
    )
  )
}
