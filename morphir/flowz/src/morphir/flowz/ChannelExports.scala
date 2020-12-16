package morphir.flowz

trait ChannelExports { channels =>
  type FlowState[+S] = StepOutputs[S, Unit]

  type FlowValue[+Output] = StepOutputs[Output, Output]

  object FlowValue {
    val unit: FlowValue[Unit] = StepOutputs((), ())
    def apply[Output](output: => Output): FlowValue[Output] =
      StepOutputs.fromValue(output)

    def fromValue[Output](output: => Output): FlowValue[Output] =
      StepOutputs.fromValue(output)
  }

  type FOuts[+S, +A] = StepOutputs[S, A]
  val FOuts: StepOutputs.type = StepOutputs

  sealed case class StepInputs[+State, +Params](state: State, params: Params) {

    def acceptingAny: StepInputs[Any, Any] = this

    def acceptingAnyState: StepInputs[Any, Params] = this

    def acceptingAnyParams: StepInputs[State, Any] = this

    /**
     * Converts the `InputChannels` to `OutputChannels`
     */
    def toOutputs: StepOutputs[State, Params] =
      StepOutputs(state = state, value = params)

    def map[P](func: Params => P): StepInputs[State, P] =
      copy(params = func(params))

    def mapState[S](func: State => S): StepInputs[S, Params] =
      copy(state = func(state))

    def tupled: (State, Params) = (state, params)

  }

  object StepInputs {

    def provideParameters[P](params: P): StepInputs[Unit, P] = setParameters(params)

    def provideState[S](state: S): StepInputs[S, Unit] = setState(state)

    def setParameters[P](params: P): StepInputs[Unit, P] =
      StepInputs(params = params, state = ())

    def setState[S](state: S): StepInputs[S, Unit] =
      StepInputs(state = state, params = ())

    val unit: StepInputs[Unit, Unit]                       = StepInputs(state = (), params = ())
    val none: StepInputs[Option[Nothing], Option[Nothing]] = StepInputs(state = None, params = None)
  }

  /**
   * A structure which contains the various output channels for a flow.
   */
  sealed case class StepOutputs[+State, +Value](value: Value, state: State) { self =>
    def map[Value2](func: Value => Value2): StepOutputs[State, Value2] =
      StepOutputs(func(value), state)

    def mapState[State2](func: State => State2): StepOutputs[State2, Value] =
      StepOutputs(value = value, state = func(state))

    /**
     * Shifts the output into the state with the output occurring on the left and the state on the right.
     */
    def toFlowStateLeft: FlowState[(Value, State)] =
      StepOutputs.fromState((value, state))

    /**
     * Shifts the output into the state with the state occurring on the left and the output on the right.
     */
    def toFlowStateRight: FlowState[(State, Value)] =
      StepOutputs.fromState((state, value))

    def toFlowValueLeft: FlowValue[(State, Value)] =
      StepOutputs.fromValue((state, value))

    def toFlowValueRight: FlowValue[(Value, State)] =
      StepOutputs.fromValue((value, state))

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

    def apply[Value](value: => Value): StepOutputs[Unit, Value] =
      StepOutputs(value = value, state = ())

    val empty: StepOutputs[Option[Nothing], Option[Nothing]] = StepOutputs(None, None)

    val none: StepOutputs[Option[Nothing], Option[Nothing]] = StepOutputs(value = None, state = None)

    val unit: StepOutputs[Unit, Unit] = StepOutputs((), ())

    def fromValue[A](value: => A): FlowValue[A] = StepOutputs(value = value, state = value)

    def fromState[State](state: => State): FlowState[State] = StepOutputs(state = state, value = ())

    def fromTuple[Value, State](tuple: (State, Value)): StepOutputs[State, Value] =
      new StepOutputs(value = tuple._2, state = tuple._1)

    def unified[Value](value: => Value): StepOutputs[Value, Value] = StepOutputs(value, value)
  }
}
