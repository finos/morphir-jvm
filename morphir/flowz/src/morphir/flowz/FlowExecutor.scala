package morphir.flowz

import zio._

abstract class FlowExecutor[+InitialState, +Params, +R <: Has[_]] {

  /**
   * The environment used for executing a flow.
   */
  def environment: Layer[Nothing, R]

  /**
   * Initialize the execution of a flow given the initial environment.
   * Part of initializing a flow involves constructing the initial state and parameters for that flow.
   */
  def initialize: RIO[FlowInitEnv, (InitialState, Params)]
}

object FlowExecutor {}
