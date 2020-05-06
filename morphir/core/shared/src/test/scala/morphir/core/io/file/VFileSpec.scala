package morphir.core.io.file

import zio.test._
import zio.test.Assertion._
import FsPath._
import zio.ZIO
object VFileSpec extends DefaultRunnableSpec {
  def spec = suite("VFile Spec")(
    suite("Creation")(
      suite("Creating with a string ")(
        testM("Must allow content retrieval of that string")(
          for {
            file <- VFile.make(currentDir </> file("hello.txt"), "Hello World!")
            text <- file.text()
          } yield assert(text)(isSome(equalTo("Hello World!")))
        )
      ),
      suite("Creating with a text producing effect")(
        testM("Should use that effect to get text")(
          for {
            file <- VFile.makeM(currentDir </> file("README.md"), ZIO.some("Hello World"))
            text <- file.text()
          } yield assert(text)(isSome(equalTo("Hello World")))
        )
      )
    )
  )
}
