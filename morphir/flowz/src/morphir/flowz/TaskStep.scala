package morphir.flowz

import zio.ZIO

object TaskStep extends FlowCompanion with AnyEnvFlowCompanion {
  def makeStep[Env, Params, Err, Out](func: Params => ZIO[Env, Err, Out]): Step[Env, Params, Err, Out] =
    Flow.parameters[Params].flatMap { params =>
      Flow.fromEffect(func(params))
    }
}
