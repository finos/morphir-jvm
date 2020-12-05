package morphir.flowz

import zio.test._
import zio.test.Assertion._

object FlowSpec extends DefaultRunnableSpec {
  def spec = suite("Flow Spec")(
    suite("Constructing")(
      testM("It should be possible to create a flow that always succeeds with the unit value")(
        for {
          output <- Flow.unit.run
        } yield assert(output)(equalTo(OutputChannels.unit))
      ),
      testM("It should be possible to create a flow that always succeeds with None")(
        for {
          output <- Flow.none.run
        } yield assert(output)(equalTo(OutputChannels.none))
      ),
      testM("It should be possible to create a flow that always succeeds with a value")(
        for {
          output <- Flow.succeed(42).run
        } yield assert(output)(equalTo(OutputChannels.fromValue(42)))
      ),
      testM("It should be possible to create a flow that always succeeds with the given output and state")(
        for {
          actual <- Flow.succeed(value = 42, state = "What is the answer?").shiftStateToOutput.run
        } yield assert(actual)(equalTo(OutputChannels.fromValue(("What is the answer?", 42))))
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
            expected = OutputChannels.fromValue(input * 2)
          } yield assert(actual)(equalTo(expected))
        }
      )
    ),
    suite("Combining")(
      testM("It should be possible to combine flows using the >>> operator.") {
        val start = Flow.parameters[List[String]]
        val next  = Flow((_: Any, args: List[String]) => (args, args.headOption))
        val flow  = start >>> next
        assertM(flow.run(List("Hello", "World")))(
          equalTo(OutputChannels(state = List("Hello", "World"), value = Option("Hello")))
        )
      }
    )
  )
}
