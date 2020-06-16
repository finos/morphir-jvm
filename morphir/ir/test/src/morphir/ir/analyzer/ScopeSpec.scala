package morphir.ir.analyzer

import zio.test._
import zio.test.Assertion._

object ScopeSpec extends DefaultRunnableSpec {
  def spec = suite("Scope Spec") {
    suite("PackageScope Spec")(
      suite("Creation")(
        test("A package scope should be creatable given some data") {
          case class PackageData(packageName: String)
          val data = PackageData("my.cool.package")

          val actual = Scope.pkg(data)
          assert(actual)(
            hasField("data", (s: PackageScope[PackageData]) => s.data, equalTo(data)) && hasField(
              "parent",
              _.parent,
              isUnit
            )
          )

        }
      )
    )
  }
}
