package morphir.flowz

import zio._

object Activity extends StepCompanion[Any] {

  def apply[Params, Value](f: Params => Value): Activity[Any, Params, Throwable, Value] =
    Step(ZIO.environment[StepContext.having.Parameters[Params]].mapEffect { ctx =>
      StepOutputs.assignBoth(f(ctx.inputs.params))
    })
}
