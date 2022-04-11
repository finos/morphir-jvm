package zio.morphir.ir

import zio.morphir.testing.MorphirBaseSpec
import zio.test.*

object ModuleNameSpec extends MorphirBaseSpec {
  import zio.morphir.ir.Module.ModuleName
  def spec: ZSpec[Environment, Any] = suite("ModuleName Spec")(
    test("fromString") {
      assertTrue(ModuleName.fromString("Basics") == ModuleName.unsafeMake()("basics"))
    }
  )
}
