package zio.morphir.ir

import zio.test.*
import zio.morphir.testing.MorphirBaseSpec

object ModuleNameSpec extends MorphirBaseSpec {
  import zio.morphir.ir.Module.ModuleName
  def spec = suite("ModuleName Spec")(
    test("fromString") {
      assertTrue(ModuleName.fromString("Basics") == ModuleName.unsafeMake()("basics"))
    }
  )
}
