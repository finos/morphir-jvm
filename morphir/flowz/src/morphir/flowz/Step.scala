package morphir.flowz

import zio.ZIO

object Step extends FlowCompanion with AnyEnvFlowCompanion {
  def environment[Env]: RStep[Env, Any, Env] =
    Flow(ZIO.environment[FlowContext.having.Environment[Env]].map(ctx => FlowValue.fromValue(ctx.environment)))
}
