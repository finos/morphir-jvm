package morphir.flowz

import zio._

object Activity extends StageCompanion[Any] {

  def apply[Params, Value](f: Params => Value): Activity[Any, Params, Throwable, Value] =
    Act(ZIO.environment[StageContext.having.Parameters[Params]].mapEffect { ctx =>
      StepOutputs.assignBoth(f(ctx.inputs.params))
    })
}
