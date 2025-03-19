package morphir.codegen.tasty
import morphir.testing.*
import zio.{ test => _, _ }
import zio.test._

object TastyToMorphirSpec extends MorphirBaseSpec:
  def spec = suite("TastyToMorphirSpec")(
    test("dummy test") {
      assertTrue(true)
    }
  )

