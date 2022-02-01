package zio.morphir.ir

import zio.Chunk
import zio.test.*

object NamingSpec extends MorphirBaseSpec {
  def spec = suite("Naming Spec")(
    fqNameSuite,
    moduleNameSuite,
    pathSuite
  )

  val fqNameSuite = suite("FQName")()

  val moduleNameSuite = suite("ModuleName")()

  val pathSuite = suite("Path")(
    test("It can be constructed from names")(
      assertTrue(
        Name("Org") / Name("Finos") == Path(Chunk(Name("Org"), Name("Finos"))),
        Name("Alpha") / Name("Beta") / Name("Gamma") == Path(Chunk(Name("Alpha"), Name("Beta"), Name("Gamma")))
      )
    )
  )
}
