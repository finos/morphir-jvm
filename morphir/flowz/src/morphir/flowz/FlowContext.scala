package morphir.flowz

final case class FlowContext[+Env, +State, +Params](environment: Env, inputs: InputChannels[State, Params])
object FlowContext {
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
  }
}
