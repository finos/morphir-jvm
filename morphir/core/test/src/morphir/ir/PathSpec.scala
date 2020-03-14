package morphir.ir

import zio.test._
import zio.test.Assertion._
import zio.test.environment._
import scala.language.implicitConversions

import upickle.default._

object PathSpec extends DefaultRunnableSpec {

  def spec = suite("PathSpec")(
    suite("Creating a Path from a String")(
      test("Should be possible when given a simple string") {
        assert(Path.fromString("Person"))(
          equalTo(Path(List(Name.fromString("person"))))
        )
      },
      test("Should be possible when given a dotted string") {
        assert(Path.fromString("blog.Author"))(
          equalTo(
            Path(
              List(Name.fromList(List("blog")), Name.fromList(List("author")))
            )
          )
        )
      }
    ),
    suite("Transforming a Path into a String")(
      test("Should be supported (a)") {
        val input =
          Path(
            Name("foo", "bar"),
            Name("baz")
          )

        assert(Path.toString(Name.toTitleCase, ".", input))(
          equalTo("FooBar.Baz")
        )
      },
      test("Should be supported (b)") {
        val input =
          Path(
            Name("foo", "bar"),
            Name("baz")
          )

        assert(Path.toString(Name.toSnakeCase, "/", input))(
          equalTo("foo_bar/baz")
        )
      }
    ),
    suite("Transforming to a list of Names")(
      test("Should be support via the toList function") {
        assert(
          Path.toList(Path(Name("Com", "Example"), Name("Hello", "World")))
        )(
          equalTo(List(Name("Com", "Example"), Name("Hello", "World")))
        )
      }
    ),
    suite("Checking if one Path is a prefix of another should:")(
      test("""Return true: Given path is "foo/bar" and prefix is "foo" """) {
        val sut = Path.fromString("foo/bar")
        val prefix = Path.fromString("foo")

        assert(Path.isPrefixOf(prefix = prefix, path = sut))(isTrue)
      },
      test("""Return false: Given path is "foo/foo" and prefix is "bar" """) {
        val sut = Path.fromString("foo/foo")
        val prefix = Path.fromString("bar")

        assert(Path.isPrefixOf(prefix = prefix, path = sut))(isFalse)
      },
      test("""Return true: Given equal paths""") {
        val sut = Path.fromString("foo/bar/baz")
        val prefix = sut

        assert(Path.isPrefixOf(prefix = prefix, path = sut))(isTrue)
      }
    ),
    suite("Path: Encoding and Decoding")(
      testM(
        "Path when encoded and then decoded should be equivalent to the original"
      ) {
        check(Gen.listOfBounded(1, 3)(Name.fuzzName)) { names =>
          val sut = Path.fromList(names)
          assert(Path.decodePath(Path.encodePath(sut)))(equalTo(sut))
        }
      },
      checkEncodesTo(
        Path.fromNames(
          Name.fromStrings("alpha"),
          Name.fromStrings("beta"),
          Name.fromStrings("gamma")
        ),
        """[["alpha"],["beta"],["gamma"]]"""
      ),
      checkEncodesTo(
        Path.fromNames(
          Name.fromStrings("alpha", "omega"),
          Name.fromStrings("beta", "delta"),
          Name.fromStrings("gamma")
        ),
        """[["alpha","omega"],["beta","delta"],["gamma"]]"""
      )
    )
  )

  def checkEncodesTo(sut: Path, expectedJsonText: String) =
    test(s"Given Path: $sut it should encode to: $expectedJsonText") {
      assert(write(Path.encodePath(sut)))(equalTo(expectedJsonText))
    }
}
