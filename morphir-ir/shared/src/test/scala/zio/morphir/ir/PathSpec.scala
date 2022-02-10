package zio.morphir.ir

import zio.Chunk
import zio.test.*
import testing.MorphirBaseSpec

object PathSpec extends MorphirBaseSpec {
  def spec = suite("Path")(
    suite("Creating a Path from a String")(
      test("It can be constructed from a simple string") {
        assertTrue(Path.fromString("Person") == Path(Chunk(Name.fromString("person"))))
      },
      test("It can be constructed from a long string") {
        assertTrue(
          Path.fromString("She Sells Seashells") == Path(
            Chunk(
              Name.fromList(List("she", "sells", "seashells"))
            )
          )
        )
      },
      test("It can be constructed when given a dotted string") {
        assertTrue(
          Path.fromString("blog.Author") == Path(
            Chunk(Name.fromList(List("blog")), Name.fromList(List("author")))
          )
        )
      }
    ),
    suite("Transforming a Path into a String")(
      test("Paths with period and TitleCase") {
        val input = Path(
          Chunk(
            Name("foo", "bar"),
            Name("baz")
          )
        )
        assertTrue(Path.toString(Name.toTitleCase, ".", input) == "FooBar.Baz")
      },
      test("Paths with slash and Snake_Case") {
        val input = Path(
          Chunk(
            Name("foo", "bar"),
            Name("baz")
          )
        )
        assertTrue(Path.toString(Name.toSnakeCase, "/", input) == "foo_bar/baz")
      }
    ),
    suite("Transforming a Path into list of Names")(
      test("It can be constructed using toList") {
        assertTrue(
          Path.toList(Path(Chunk(Name("Com", "Example"), Name("Hello", "World"))))
            == List(Name("Com", "Example"), Name("Hello", "World"))
        )
      }
    ),
    suite("Creating a Path from a Name")(
      test("It can be constructed from names")(
        assertTrue(
          Name("Org") / Name("Finos") == Path(Chunk(Name("Org"), Name("Finos"))),
          Name("Alpha") / Name("Beta") / Name("Gamma") == Path(Chunk(Name("Alpha"), Name("Beta"), Name("Gamma")))
        )
      )
    ),
    suite("Checking if one Path is a prefix of another should:")(
      test("""Return true: Given path is "foo/bar" and prefix is "foo" """) {
        val sut    = Path.fromString("foo/bar")
        val prefix = Path.fromString("foo")
        val x      = Path.isPrefixOf(prefix = prefix, path = sut)
        assertTrue(x)
      },
      test("""Return false: Given path is "foo/foo" and prefix is "bar" """) {
        val sut    = Path.fromString("foo/foo")
        val prefix = Path.fromString("bar")

        val x = Path.isPrefixOf(prefix = prefix, path = sut)
        assertTrue(!x)
      },
      test("""Return true: Given equal paths""") {
        val sut    = Path.fromString("foo/bar/baz")
        val prefix = sut
        val x      = Path.isPrefixOf(prefix = prefix, path = sut)
        assertTrue(x)
      }
    )
  )
}
