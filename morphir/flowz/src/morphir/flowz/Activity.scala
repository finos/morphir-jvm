package morphir.flowz

import zio._

object Activity extends ActCompanion[Any] {

  def apply[Params, Value](f: Params => Value): Activity[Any, Params, Throwable, Value] =
    Act(ZIO.environment[ActContext.having.Parameters[Params]].mapEffect { ctx =>
      StepOutputs.assignBoth(f(ctx.inputs.params))
    })
}