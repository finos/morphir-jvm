package morphir.flowz

import zio.test._
import zio.test.Assertion._

object FlowSpec extends DefaultRunnableSpec {
  def spec = suite("Flow Spec")(
    suite("Construction")(
      testM("It should be possible to create a flow that always succeeds with a value")(
        for {
          output <- Flow.succeed(42).run
        } yield assert(output)(equalTo(FlowSuccess.fromOutput(42)))
      )
    )
  )
}
