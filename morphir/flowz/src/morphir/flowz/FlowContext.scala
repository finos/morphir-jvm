package morphir.flowz

final case class FlowContext[+Env, +State, +Params](environment: Env, inputs: InputChannels[State, Params]) { self =>

  def updateInputs[S, P](state: S, params: P): FlowContext[Env, S, P] =
    self.copy(inputs = InputChannels(state = state, params = params))

  def updateInputs[S, A](outputs: OutputChannels[S, A]): FlowContext[Env, S, A] =
    self.copy(inputs = outputs.toInputs)

  def updateState[S](state: S): FlowContext[Env, S, Params] =
    self.copy(inputs = self.inputs.copy(state = state))

}
object FlowContext {
  def apply[Env, State, Params](environment: Env, state: State, params: Params): FlowContext[Env, State, Params] =
    FlowContext(environment = environment, inputs = InputChannels(params = params, state = state))

  def provideEnvironment[Env](env: => Env): FlowContext[Env, Unit, Unit] =
    setEnvironment(env)

  def setEnvironment[Env](env: => Env): FlowContext[Env, Unit, Unit] =
    FlowContext(environment = env, inputs = InputChannels.unit)

  object having {

    /**
     * A `FlowContext` which accepts any inputs.
     */
    type AnyInputs = FlowContext[Any, Any, Any]

    /**
     * A `FlowContext` which accepts any environment.
     */
    type AnyEnv[+State, +Params] = FlowContext[Any, State, Params]

    /**
     * A `FlowContext` which accepts any state.
     */
    type AnyState[+Env, +Params] = FlowContext[Env, Any, Params]

    /**
     * A `FlowContext` which accepts any parameters
     */
    type AnyParams[+Env, +State] = FlowContext[Env, State, Any]

    /**
     * A `FlowContext` whose environment must be compatible with type `R`.
     */
    type EnvOf[+R]    = FlowContext[R, Any, Any]
    type StateOf[+S]  = FlowContext[Any, S, Any]
    type ParamsOf[+P] = FlowContext[Any, Any, P]
  }
}
