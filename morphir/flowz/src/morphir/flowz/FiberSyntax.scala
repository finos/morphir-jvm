package morphir.flowz

import zio.Fiber

trait FiberSyntax {
  import FiberSyntax._
  implicit def toFiberOutputChannelOps[State, Err, Output](
    fiber: Fiber[Err, StepOutputs[State, Err]]
  ): FiberOutputChannelOps[State, Err, Output] =
    new FiberOutputChannelOps[State, Err, Output](fiber)

}

object FiberSyntax {
  class FiberOutputChannelOps[+State, +Err, +Output](val self: Fiber[Err, StepOutputs[State, Err]]) extends {
    def joinFlow: Step[Any, State, Any, Any, Err, Err] = Step.join(self)
  }
}
