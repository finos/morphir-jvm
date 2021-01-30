package morphir.flowz

final case class ActContext[+Env, +State, +Params](environment: Env, inputs: StepInputs[State, Params]) { self =>

  def updateInputs[S, P](state: S, params: P): ActContext[Env, S, P] =
    self.copy(inputs = StepInputs(state = state, params = params))

  def updateInputs[S, A](outputs: StepOutputs[S, A]): ActContext[Env, S, A] =
    self.copy(inputs = outputs.toInputs)

  def updateParams[P](params: P): ActContext[Env, State, P] =
    self.copy(inputs = self.inputs.copy(params = params))

  def updateState[S](state: S): ActContext[Env, S, Params] =
    self.copy(inputs = self.inputs.copy(state = state))

  def toOutputs: StepOutputs[State, Params] =
    StepOutputs(state = inputs.state, value = inputs.params)

  def toOutputs[A](value: => A): StepOutputs[State, A] =
    StepOutputs(state = inputs.state, value = value)
}

object ActContext {

  val unit: ActContext[Any, Any, Any] =
    new ActContext(environment = (), inputs = StepInputs(state = (), params = ()))
  @inline val any: ActContext[Any, Any, Any] = ActContext.unit

  def apply[Env, State, Params](environment: Env, state: State, params: Params): ActContext[Env, State, Params] =
    ActContext(environment = environment, inputs = StepInputs(params = params, state = state))

  def fromEnvironment[Env](environment: Env): ActContext[Env, Any, Any] =
    ActContext(environment, inputs = StepInputs.AnyInputs)

  def fromParams[Params](params: Params): ActContext[Any, Any, Params] =
    ActContext(environment = (): Any, inputs = StepInputs.fromParams(params))

  def fromState[State](state: State): ActContext[Any, State, Any] =
    ActContext(environment = (): Any, inputs = StepInputs.fromState(state))

  def provideEnvironment[Env](env: => Env): ActContext[Env, Unit, Unit] =
    setEnvironment(env)

  def setEnvironment[Env](env: => Env): ActContext[Env, Unit, Unit] =
    ActContext(environment = env, inputs = StepInputs.unit)

  object having {

    /**
     * A `StepContext` which accepts any inputs.
     */
    type AnyInputs = ActContext[Any, Any, Any]

    /**
     * A `StepContext` which accepts any environment.
     */
    type AnyEnv[+State, +Params] = ActContext[Any, State, Params]

    /**
     * A `StepContext` which accepts any state.
     */
    type AnyState[+Env, +Params] = ActContext[Env, Any, Params]

    /**
     * A `StepContext` which accepts any parameters
     */
    type AnyParams[+Env, +State] = ActContext[Env, State, Any]

    /**
     * A `StepContext` whose environment must be compatible with type `R`.
     */
    type Environment[+R] = ActContext[R, Any, Any]
    type InputState[+S]  = ActContext[Any, S, Any]
    type Parameters[+P]  = ActContext[Any, Any, P]
  }
}
