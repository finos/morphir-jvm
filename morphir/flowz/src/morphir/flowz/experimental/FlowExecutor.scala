package morphir.flowz.experimental

import morphir.flowz.{ ExecutableFlow, Properties }
import zio.{ ExecutionStrategy, Has, Layer, RIO, UIO }

abstract class FlowExecutor[+InitialState, +Msg, +R <: Has[_], E] {
  def run(flow: ExecutableFlow[InitialState, Msg, R, E], executionStrategy: ExecutionStrategy): UIO[ExecutedFlow[E]]
  def initialize: RIO[FlowBaseEnv, (InitialState, Msg)]
  def environment: Layer[Nothing, R]
}

object FlowExecutor {
  def default[SIn, Msg, R <: Properties, E](init: RIO[FlowBaseEnv, (SIn, Msg)])(
    env: Layer[Nothing, R]
  ): FlowExecutor[SIn, Msg, R, E] = new FlowExecutor[SIn, Msg, R, E] {
    def run(flow: ExecutableFlow[SIn, Msg, R, E], executionStrategy: ExecutionStrategy): UIO[ExecutedFlow[E]] =
      ???

    val initialize: RIO[FlowBaseEnv, (SIn, Msg)] = init

    val environment: Layer[Nothing, R] = env
  }
}
