package morphir.flowz

final case class FlowSuccess[+Output, +State](output: Output, state: State) { self =>
  def map[Output2](func: Output => Output2): FlowSuccess[Output2, State] =
    FlowSuccess(func(output), state)

  def mapState[State2](func: State => State2): FlowSuccess[Output, State2] =
    FlowSuccess(output = output, state = func(state))

}
object FlowSuccess {

  def apply[Output, State](tuple: (Output, State)): FlowSuccess[Output, State] =
    fromTuple(tuple)

  val none: FlowSuccess[Option[Nothing], Option[Nothing]] = FlowSuccess(None, None)

  val unit: FlowSuccess[Unit, Unit] = FlowSuccess((), ())

  def fromOutput[Out](output: => Out): FlowSuccess[Out, Unit] = FlowSuccess(output = output, state = ())

  def fromState[State](state: => State): FlowSuccess[Unit, State] = FlowSuccess(state = state, output = ())

  def fromTuple[Output, State](tuple: (Output, State)): FlowSuccess[Output, State] =
    new FlowSuccess(output = tuple._1, state = tuple._2)
}
