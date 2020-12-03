package morphir.flowz

import zio.test._
import zio.test.Assertion._

object FlowSpec extends DefaultRunnableSpec {
  def spec = suite("Flow Spec")(
    suite("Construction")(
      testM("It should be possible to create a flow that always succeeds with the unit value")(
        for {
          output <- Flow.unit.run
        } yield assert(output)(equalTo(FlowOutputs.unit))
      ),
      testM("It should be possible to create a flow that always succeeds with None")(
        for {
          output <- Flow.none.run
        } yield assert(output)(equalTo(FlowOutputs.none))
      ),
      testM("It should be possible to create a flow that always succeeds with a value")(
        for {
          output <- Flow.succeed(42).run
        } yield assert(output)(equalTo(FlowOutputs.fromOutput(42)))
      ),
      testM("It should be possible to create a flow that always succeeds with the given output and state")(
        for {
          actual <- Flow.succeed(output = 42, state = "What is the answer?").shiftStateToOutput.run
        } yield assert(actual)(equalTo(FlowOutputs.fromOutput(("What is the answer?", 42))))
      ),
      testM("It should be possible to create a flow that always fails with a value")(
        for {
          result <- Flow.fail("NO!!!").run.run
        } yield assert(result)(fails(equalTo("NO!!!")))
      ),
      testM("It should be possible to create a flow that produces the value of executing a function")(
        checkM(Gen.int(1, 5000)) { input =>
          for {
            actual  <- Flow.fromFunction { n: Int => n * 2 }.run(input)
            expected = FlowOutputs.fromOutput(input * 2)
          } yield assert(actual)(equalTo(expected))
        }
      )
    )
  )
}
