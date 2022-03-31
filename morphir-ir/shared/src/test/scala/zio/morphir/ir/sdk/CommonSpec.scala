package zio.morphir.ir.sdk

import zio.morphir.ir.{Gens, Name, Path}
import zio.morphir.ir.Type.Type
import zio.morphir.testing.MorphirBaseSpec
import zio.test._

object CommonSpec extends MorphirBaseSpec {
  def spec =
    suite("Common Spec")(
      suite("packageName")(
        test("should return the expected value") {
          assertTrue(Common.packageName.toPath == Path.fromString("Morphir.SDK"))
        }
      ),
      suite("tVar")(
        test("should work as expected") {
          check(Gens.words) { s =>
            val actual = Common.tVar(s)
            assertTrue(actual.satisfiesCaseOf { case Type.Variable(_, name) =>
              name == Name.fromString(s)
            })
          }
        }
      )
    )
}
