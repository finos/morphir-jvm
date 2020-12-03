package morphir.flowz

final case class FlowSuccess[+Output, +State](output: Output, state: State) { self =>
  def map[Output2](func: Output => Output2): FlowSuccess[Output2, State] =
    FlowSuccess(func(output), state)

  def mapState[State2](func: State => State2): FlowSuccess[Output, State2] =
    FlowSuccess(output = output, state = func(state))

  /**
   * Shifts the output into the state with the output occurring on the left and the state on the right.
   */
  def toFlowStateLeft: FlowState[(Output, State)] =
    FlowSuccess.fromState((output, state))

  /**
   * Shifts the output into the state with the state occurring on the left and the output on the right.
   */
  def toFlowStateRight: FlowState[(State, Output)] =
    FlowSuccess.fromState((state, output))

  def toFlowOutputLeft: FlowOutput[(State, Output)] =
    FlowSuccess.fromOutput((state, output))

  def toFlowOutputRight: FlowOutput[(Output, State)] =
    FlowSuccess.fromOutput((output, state))

  def transform[State2, Output2](func: (State, Output) => (State2, Output2)): FlowSuccess[Output2, State2] = {
    val (newState, newValue) = func(self.state, self.output)
    FlowSuccess(state = newState, output = newValue)
  }

  def toTuple: (State, Output) = (self.state, self.output)

  def zip[Output2, State2](that: FlowSuccess[Output2, State2]): FlowSuccess[(Output, Output2), (State, State2)] =
    FlowSuccess(output = (self.output, that.output), state = (self.state, that.state))

}
object FlowSuccess {

  val empty: FlowSuccess[Option[Nothing], Option[Nothing]] = FlowSuccess(None, None)

  val none: FlowSuccess[Option[Nothing], Unit] = FlowSuccess(None, ())

  val unit: FlowSuccess[Unit, Unit] = FlowSuccess((), ())

  def fromOutput[Out](output: => Out): FlowOutput[Out] = FlowSuccess(output = output, state = ())

  def fromState[State](state: => State): FlowState[State] = FlowSuccess(state = state, output = ())

  def fromTuple[Output, State](tuple: (Output, State)): FlowSuccess[Output, State] =
    new FlowSuccess(output = tuple._1, state = tuple._2)
}
