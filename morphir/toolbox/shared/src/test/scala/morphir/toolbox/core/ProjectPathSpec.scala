package morphir.toolbox.core

import toml.Codecs._
import toml.Toml
import zio.test.Assertion.{ equalTo, isRight }
import zio.test.{ assert, suite, test, DefaultRunnableSpec }

object ProjectPathSpec extends DefaultRunnableSpec {
  def spec = suite("ProjectPath Spec")(
    suite("TOML Codec")(
      test("A path-like string should decode from TOML") {
        case class Root(path: ProjectPath)
        val tomlDoc = """ path = "foo/bar/baz" """
        val result  = Toml.parseAs[Root](tomlDoc)
        assert(result)(
          isRight(equalTo(Root(ProjectPath.of("foo", "bar", "baz"))))
        )
      }
    )
  )
}
