package morphir.flowz

import zio.{ ExecutionStrategy, Has, Layer, RIO, UIO }

abstract class FlowExecutor[+InitialState, +Msg, +R <: Has[_], E] {
  def run(flow: ExecutableFlow[InitialState, Msg, R, E], executionStrategy: ExecutionStrategy): UIO[ExecutedFlow[E]]
  def initialize: RIO[FlowBaseEnv, (InitialState, Msg)]
  def environment: Layer[Nothing, R]
}
