package morphir.flowz

import zio.{ ExecutionStrategy, Has, Layer, RIO, UIO }

abstract class FlowExecutor[InitialState, Msg, +R <: Has[_], E] {
  def run(flow: ExecutableFlow[InitialState, Msg, R, E], executionStrategy: ExecutionStrategy)(
    initialize: RIO[FlowBaseEnv, (InitialState, Msg)]
  ): UIO[ExecutedFlow[E]]

  def environment: Layer[Nothing, R]
}
