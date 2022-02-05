package zio.morphir.ir

import zio.Chunk
import zio.test.*
import testing.MorphirBaseSpec

object PathSpec extends MorphirBaseSpec {
  def spec = suite("Path")(
    test("It can be constructed from names")(
      assertTrue(
        Name("Org") / Name("Finos") == Path(Chunk(Name("Org"), Name("Finos"))),
        Name("Alpha") / Name("Beta") / Name("Gamma") == Path(Chunk(Name("Alpha"), Name("Beta"), Name("Gamma")))
      )
    )
  )
}
