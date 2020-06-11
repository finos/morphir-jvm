package morphir.ir

import morphir.ir.documented._
import morphir.ir.testing.JsonSpec
import zio.test._
import zio.test.TestAspect._

object DocumentedSpec extends DefaultRunnableSpec with JsonSpec {
  def spec =
    suite("Documented Spec")(
      suite("JSON Codec")(
        testM("The JSON Codec should be well behaving") {
          val data     = Map("Dog" -> "Snoopy", "Cat" -> "Garfield", "Pig" -> "Babe")
          val original = Documented("This is documentation.", data)
          checkCodecIsWellBehaved(original)
        } //@@ debug
      )
    ) @@ silent
}
