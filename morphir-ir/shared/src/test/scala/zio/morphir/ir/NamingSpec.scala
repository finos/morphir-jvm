package zio.morphir.ir

import zio.test.*
object NamingSpec extends MorphirBaseSpec {
  def spec = suite("Naming Spec")(
    fqNameSuite
  )

  val fqNameSuite = suite("FQName")()
}
