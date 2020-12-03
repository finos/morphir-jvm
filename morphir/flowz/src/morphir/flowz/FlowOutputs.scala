package morphir.flowz

final case class FlowOutputs[+State, +Output](output: Output, state: State) { self =>
  def map[Output2](func: Output => Output2): FlowOutputs[State, Output2] =
    FlowOutputs(func(output), state)

  def mapState[State2](func: State => State2): FlowOutputs[State2, Output] =
    FlowOutputs(output = output, state = func(state))

  /**
   * Shifts the output into the state with the output occurring on the left and the state on the right.
   */
  def toFlowStateLeft: FlowState[(Output, State)] =
    FlowOutputs.fromState((output, state))

  /**
   * Shifts the output into the state with the state occurring on the left and the output on the right.
   */
  def toFlowStateRight: FlowState[(State, Output)] =
    FlowOutputs.fromState((state, output))

  def toFlowOutputLeft: FlowOutput[(State, Output)] =
    FlowOutputs.fromOutput((state, output))

  def toFlowOutputRight: FlowOutput[(Output, State)] =
    FlowOutputs.fromOutput((output, state))

  def transform[State2, Output2](func: (State, Output) => (State2, Output2)): FlowOutputs[State2, Output2] = {
    val (newState, newValue) = func(self.state, self.output)
    FlowOutputs(state = newState, output = newValue)
  }

  def toTuple: (State, Output) = (self.state, self.output)

  def zip[State2, Output2](that: FlowOutputs[State2, Output2]): FlowOutputs[(State, State2), (Output, Output2)] =
    FlowOutputs(output = (self.output, that.output), state = (self.state, that.state))

}
object FlowOutputs {

  val empty: FlowOutputs[Option[Nothing], Option[Nothing]] = FlowOutputs(None, None)

  val none: FlowOutputs[Unit, Option[Nothing]] = FlowOutputs(output = None, state = ())

  val unit: FlowOutputs[Unit, Unit] = FlowOutputs((), ())

  def fromOutput[Out](output: => Out): FlowOutput[Out] = FlowOutputs(output = output, state = ())

  def fromState[State](state: => State): FlowState[State] = FlowOutputs(state = state, output = ())

  def fromTuple[Output, State](tuple: (State, Output)): FlowOutputs[State, Output] =
    new FlowOutputs(output = tuple._2, state = tuple._1)
}
