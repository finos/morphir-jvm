package morphir.flowz

/**
 * A structure which contains the various output channels for a flow.
 */
final case class OutputChannels[+State, +Value](value: Value, state: State) { self =>
  def map[Value2](func: Value => Value2): OutputChannels[State, Value2] =
    OutputChannels(func(value), state)

  def mapState[State2](func: State => State2): OutputChannels[State2, Value] =
    OutputChannels(value = value, state = func(state))

  /**
   * Shifts the output into the state with the output occurring on the left and the state on the right.
   */
  def toFlowStateLeft: FlowState[(Value, State)] =
    OutputChannels.fromState((value, state))

  /**
   * Shifts the output into the state with the state occurring on the left and the output on the right.
   */
  def toFlowStateRight: FlowState[(State, Value)] =
    OutputChannels.fromState((state, value))

  def toFlowValueLeft: FlowValue[(State, Value)] =
    OutputChannels.fromValue((state, value))

  def toFlowValueRight: FlowValue[(Value, State)] =
    OutputChannels.fromValue((value, state))

  def transform[State2, Value2](func: (State, Value) => (State2, Value2)): OutputChannels[State2, Value2] = {
    val (newState, newValue) = func(self.state, self.value)
    OutputChannels(state = newState, value = newValue)
  }

  def toInputs: InputChannels[State, Value] =
    InputChannels(state = self.state, params = self.value)

  def toTuple: (State, Value) = (self.state, self.value)

  def zip[State2, Output2](that: OutputChannels[State2, Output2]): OutputChannels[(State, State2), (Value, Output2)] =
    OutputChannels(value = (self.value, that.value), state = (self.state, that.state))

}
object OutputChannels {

  val empty: OutputChannels[Option[Nothing], Option[Nothing]] = OutputChannels(None, None)

  val none: OutputChannels[Unit, Option[Nothing]] = OutputChannels(value = None, state = ())

  val unit: OutputChannels[Unit, Unit] = OutputChannels((), ())

  def fromValue[A](value: => A): FlowValue[A] = OutputChannels(value = value, state = ())

  def fromState[State](state: => State): FlowState[State] = OutputChannels(state = state, value = ())

  def fromTuple[Value, State](tuple: (State, Value)): OutputChannels[State, Value] =
    new OutputChannels(value = tuple._2, state = tuple._1)
}
