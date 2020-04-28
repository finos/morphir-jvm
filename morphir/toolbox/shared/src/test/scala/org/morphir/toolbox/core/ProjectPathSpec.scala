package org.morphir.toolbox.core

import zio.test._
import zio.test.Assertion._
import toml.Codecs._
import toml._
import org.morphir.toolbox.core.codecs.TomlSupport._

object ProjectPathSpec extends DefaultRunnableSpec {
  def spec = suite("ProjectPath Spec")(
    suite("TOML Codec")(
      test("A path-like string should decode from TOML") {
        case class Root(path: ProjectPath)
        val tomlDoc = """ path = "foo/bar/baz" """
        val result = Toml.parseAs[Root](tomlDoc)
        assert(result)(
          isRight(equalTo(Root(ProjectPath.of("foo", "bar", "baz"))))
        )
      }
    )
  )
}
