package morphir.ir

import morphir.ir.Path.Path
import zio.test.Assertion._

import morphir.testing.MorphirBaseSpec
import zio.test._
object PathSpec extends MorphirBaseSpec {
  def spec = suite("PathSpec")(
    suite("Path Utility Functions Tests")(
      test("fromString Test") {
        val pathFromString     = Path.fromString("fooBar.baz")
        val expectedPath: Path = List(List("foo", "bar"), List("baz"))

        assert(pathFromString)(equalTo(expectedPath))
      }
    )
  )
}
