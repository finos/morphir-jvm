package morphir.flowz

final case class StepContext[+Env, +State, +Params](environment: Env, inputs: StepInputs[State, Params]) { self =>

  def updateInputs[S, P](state: S, params: P): StepContext[Env, S, P] =
    self.copy(inputs = StepInputs(state = state, params = params))

  def updateInputs[S, A](outputs: StepOutputs[S, A]): StepContext[Env, S, A] =
    self.copy(inputs = outputs.toInputs)

  def updateParams[P](params: P): StepContext[Env, State, P] =
    self.copy(inputs = self.inputs.copy(params = params))

  def updateState[S](state: S): StepContext[Env, S, Params] =
    self.copy(inputs = self.inputs.copy(state = state))
}

object StepContext {

  val unit: StepContext[Any, Any, Any]        = new StepContext(environment = (), inputs = StepInputs(state = (), params = ()))
  @inline val any: StepContext[Any, Any, Any] = StepContext.unit

  def apply[Env, State, Params](environment: Env, state: State, params: Params): StepContext[Env, State, Params] =
    StepContext(environment = environment, inputs = StepInputs(params = params, state = state))

  def fromEnvironment[Env](environment: Env): StepContext[Env, Any, Any] =
    StepContext(environment, inputs = StepInputs.AnyInputs)

  def fromParams[Params](params: Params): StepContext[Any, Any, Params] =
    StepContext(environment = (): Any, inputs = StepInputs.fromParams(params))

  def fromState[State](state: State): StepContext[Any, State, Any] =
    StepContext(environment = (): Any, inputs = StepInputs.fromState(state))

  def provideEnvironment[Env](env: => Env): StepContext[Env, Unit, Unit] =
    setEnvironment(env)

  def setEnvironment[Env](env: => Env): StepContext[Env, Unit, Unit] =
    StepContext(environment = env, inputs = StepInputs.unit)

  object having {

    /**
     * A `StepContext` which accepts any inputs.
     */
    type AnyInputs = StepContext[Any, Any, Any]

    /**
     * A `StepContext` which accepts any environment.
     */
    type AnyEnv[+State, +Params] = StepContext[Any, State, Params]

    /**
     * A `StepContext` which accepts any state.
     */
    type AnyState[+Env, +Params] = StepContext[Env, Any, Params]

    /**
     * A `StepContext` which accepts any parameters
     */
    type AnyParams[+Env, +State] = StepContext[Env, State, Any]

    /**
     * A `StepContext` whose environment must be compatible with type `R`.
     */
    type Environment[+R] = StepContext[R, Any, Any]
    type InputState[+S]  = StepContext[Any, S, Any]
    type Parameters[+P]  = StepContext[Any, Any, P]
  }
}
