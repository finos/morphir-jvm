package morphir.flowz

import zio.test._
import zio.test.Assertion._

object FlowHostSpec extends DefaultRunnableSpec {
  def spec = suite("FlowHost Spec")(
    testM()
  )
}
