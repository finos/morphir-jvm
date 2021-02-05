package morphir.flowz

import zio.{ ExecutionStrategy, Has, Layer, RIO, UIO }

abstract class FlowExecutor[+InitialState, +Trg, +R <: Has[_], E] {
  def run(flow: ExecutableFlow[InitialState, Trg, R, E], executionStrategy: ExecutionStrategy): UIO[ExecutedFlow[E]]
  def initialize: RIO[FlowBaseEnv, (InitialState, Trg)]
  def environment: Layer[Nothing, R]
}
