package morphir.flowz

import zio._

abstract class AbstractRunnableFlow {
  type InputState
  type OutputState
  type Message
  type Environment <: Has[_]
  type Failure
  type Success

  def flow: Flow[InputState, OutputState, Message, Environment, Failure, Success]
  private[flowz] def runFlow(flow: Flow[InputState, OutputState, Message, Environment, Failure, Success]) =
    //TODO: Complete definition following the example from zio-test
    flow

}
