package morphir.ir

import morphir.ir.Path.Path
import zio.test.Assertion._
import zio.test._

object PathSpec extends DefaultRunnableSpec {
  override def spec: ZSpec[_root_.zio.test.environment.TestEnvironment, Any] = suite("Path Utility Functions Tests")(
    test("fromString Test") {
      val pathFromString     = Path.fromString("fooBar.baz")
      val expectedPath: Path = List(List("foo", "bar"), List("baz"))

      assert(pathFromString)(equalTo(expectedPath))
    }
  )
}
