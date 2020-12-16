package morphir.flowz

import zio.test._
import zio.test.Assertion._

object ConstantStepSpec extends DefaultRunnableSpec {
  def spec = suite("ConstantStep Spec")(
    testM("It should produce a constant when created using succeed") {
      val sut = ConstantStep.succeed(42)
      assertM(sut.run)(equalTo(StepOutputs.fromValue(42)))
    },
    testM("It should produce a constant value and state when created using succeed") {
      val sut = ConstantStep.succeed(state = List(1), value = 2)
      assertM(sut.run)(equalTo(StepOutputs(state = List(1), value = 2)))
    }
  )
}
