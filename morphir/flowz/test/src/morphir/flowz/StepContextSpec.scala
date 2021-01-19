package morphir.flowz

import zio.test._
import zio.test.Assertion._
object StepContextSpec extends DefaultRunnableSpec {
  def spec = suite("StepContext Spec")(
    suite("When Constructing a StepContext")(
      test("It should be possible to create one given only Params")(
        assert(StepContext.fromEnvironment(42))(equalTo(StepContext(environment = 42, state = (), params = ())))
      )
    )
  )
}
