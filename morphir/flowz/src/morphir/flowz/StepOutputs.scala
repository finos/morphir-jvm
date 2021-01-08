package morphir.flowz

final case class StepOutputs[+State, +Value](state: State, value: Value) { self =>
  def map[Value2](func: Value => Value2): StepOutputs[State, Value2] =
    StepOutputs(value = func(value), state = state)

  def mapState[State2](func: State => State2): StepOutputs[State2, Value] =
    StepOutputs(value = value, state = func(state))

  def transform[State2, Value2](func: (State, Value) => (State2, Value2)): StepOutputs[State2, Value2] = {
    val (newState, newValue) = func(self.state, self.value)
    StepOutputs(state = newState, value = newValue)
  }

  def toInputs: StepInputs[State, Value] =
    StepInputs(state = self.state, params = self.value)

  def toTuple: (State, Value) = (self.state, self.value)

  def zip[State2, Output2](that: StepOutputs[State2, Output2]): StepOutputs[(State, State2), (Value, Output2)] =
    StepOutputs(value = (self.value, that.value), state = (self.state, that.state))

}

object StepOutputs {
  def assignBoth[A](value: A): StepOutputs[A, A] = unified(value)

  def fromState[State](state: => State): StepOutputs[State, Unit] =
    StepOutputs(state = state, value = ())

  def fromValue[Value](value: => Value): StepOutputs[Unit, Value] =
    StepOutputs(state = (), value = value)

  def apply[Value](value: => Value): StepOutputs[Unit, Value] =
    StepOutputs(value = value, state = ())

  val empty: StepOutputs[Option[Nothing], Option[Nothing]] = StepOutputs(None, None)

  val none: StepOutputs[Option[Nothing], Option[Nothing]] = StepOutputs(value = None, state = None)

  val unit: StepOutputs[Unit, Unit] = StepOutputs((), ())

  def fromTuple[Value, State](tuple: (State, Value)): StepOutputs[State, Value] =
    new StepOutputs(value = tuple._2, state = tuple._1)

  def setBoth[A](value: A): StepOutputs[A, A] = unified(value)

  def unified[Value](value: Value): StepOutputs[Value, Value] = StepOutputs(value, value)
}
