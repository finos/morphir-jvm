package morphir.flowz

trait ChannelExports { channels =>
  type FlowState[+S] = OutputChannels[S, Unit]

  type FlowValue[+Output] = OutputChannels[Output, Output]

  object FlowValue {
    val unit: FlowValue[Unit] = OutputChannels((), ())
    def apply[Output](output: => Output): FlowValue[Output] =
      OutputChannels.fromValue(output)

    def fromValue[Output](output: => Output): FlowValue[Output] =
      OutputChannels.fromValue(output)
  }

  type FOuts[+S, +A] = OutputChannels[S, A]
  val FOuts: OutputChannels.type = OutputChannels

  sealed case class InputChannels[+State, +Params](state: State, params: Params) {

    def acceptingAny: InputChannels[Any, Any] = this

    def acceptingAnyState: InputChannels[Any, Params] = this

    def acceptingAnyParams: InputChannels[State, Any] = this

    /**
     * Converts the `InputChannels` to `OutputChannels`
     */
    def toOutputs: OutputChannels[State, Params] =
      OutputChannels(state = state, value = params)

    def map[P](func: Params => P): InputChannels[State, P] =
      copy(params = func(params))

    def mapState[S](func: State => S): InputChannels[S, Params] =
      copy(state = func(state))

  }

  object InputChannels {

    def provideParameters[P](params: P): InputChannels[Unit, P] = setParameters(params)

    def provideState[S](state: S): InputChannels[S, Unit] = setState(state)

    def setParameters[P](params: P): InputChannels[Unit, P] =
      InputChannels(params = params, state = ())

    def setState[S](state: S): InputChannels[S, Unit] =
      InputChannels(state = state, params = ())

    val unit: InputChannels[Unit, Unit]                       = InputChannels(state = (), params = ())
    val none: InputChannels[Option[Nothing], Option[Nothing]] = InputChannels(state = None, params = None)
  }

  /**
   * A structure which contains the various output channels for a flow.
   */
  sealed case class OutputChannels[+State, +Value](value: Value, state: State) { self =>
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

    def apply[Value](value: => Value): OutputChannels[Unit, Value] =
      OutputChannels(value = value, state = ())

    val empty: OutputChannels[Option[Nothing], Option[Nothing]] = OutputChannels(None, None)

    val none: OutputChannels[Option[Nothing], Option[Nothing]] = OutputChannels(value = None, state = None)

    val unit: OutputChannels[Unit, Unit] = OutputChannels((), ())

    def fromValue[A](value: => A): FlowValue[A] = OutputChannels(value = value, state = value)

    def fromState[State](state: => State): FlowState[State] = OutputChannels(state = state, value = ())

    def fromTuple[Value, State](tuple: (State, Value)): OutputChannels[State, Value] =
      new OutputChannels(value = tuple._2, state = tuple._1)

    def unified[Value](value: => Value): OutputChannels[Value, Value] = OutputChannels(value, value)
  }
}
