package morphir.flowz

final case class StageContext[+Env, +State, +Params](environment: Env, inputs: StepInputs[State, Params]) { self =>

  def updateInputs[S, P](state: S, params: P): StageContext[Env, S, P] =
    self.copy(inputs = StepInputs(state = state, params = params))

  def updateInputs[S, A](outputs: StepOutputs[S, A]): StageContext[Env, S, A] =
    self.copy(inputs = outputs.toInputs)

  def updateParams[P](params: P): StageContext[Env, State, P] =
    self.copy(inputs = self.inputs.copy(params = params))

  def updateState[S](state: S): StageContext[Env, S, Params] =
    self.copy(inputs = self.inputs.copy(state = state))

  def toOutputs: StepOutputs[State, Params] =
    StepOutputs(state = inputs.state, value = inputs.params)

  def toOutputs[A](value: => A): StepOutputs[State, A] =
    StepOutputs(state = inputs.state, value = value)
}

object StageContext {

  val unit: StageContext[Any, Any, Any] =
    new StageContext(environment = (), inputs = StepInputs(state = (), params = ()))
  @inline val any: StageContext[Any, Any, Any] = StageContext.unit

  def apply[Env, State, Params](environment: Env, state: State, params: Params): StageContext[Env, State, Params] =
    StageContext(environment = environment, inputs = StepInputs(params = params, state = state))

  def fromEnvironment[Env](environment: Env): StageContext[Env, Any, Any] =
    StageContext(environment, inputs = StepInputs.AnyInputs)

  def fromParams[Params](params: Params): StageContext[Any, Any, Params] =
    StageContext(environment = (): Any, inputs = StepInputs.fromParams(params))

  def fromState[State](state: State): StageContext[Any, State, Any] =
    StageContext(environment = (): Any, inputs = StepInputs.fromState(state))

  def provideEnvironment[Env](env: => Env): StageContext[Env, Unit, Unit] =
    setEnvironment(env)

  def setEnvironment[Env](env: => Env): StageContext[Env, Unit, Unit] =
    StageContext(environment = env, inputs = StepInputs.unit)

  object having {

    /**
     * A `StepContext` which accepts any inputs.
     */
    type AnyInputs = StageContext[Any, Any, Any]

    /**
     * A `StepContext` which accepts any environment.
     */
    type AnyEnv[+State, +Params] = StageContext[Any, State, Params]

    /**
     * A `StepContext` which accepts any state.
     */
    type AnyState[+Env, +Params] = StageContext[Env, Any, Params]

    /**
     * A `StepContext` which accepts any parameters
     */
    type AnyParams[+Env, +State] = StageContext[Env, State, Any]

    /**
     * A `StepContext` whose environment must be compatible with type `R`.
     */
    type Environment[+R] = StageContext[R, Any, Any]
    type InputState[+S]  = StageContext[Any, S, Any]
    type Parameters[+P]  = StageContext[Any, Any, P]
  }
}
