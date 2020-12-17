package morphir.flowz

final case class StepInputs[+State, +Params](state: State, params: Params) { self =>
  def acceptingAny: StepInputs[Any, Any] = this

  def acceptingAnyState: StepInputs[Any, Params] = this

  def acceptingAnyParams: StepInputs[State, Any] = this

  /**
   * Converts the `StepInputs` to `StepOutputs`
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

  type AnyInputs = StepInputs[Any, Any]
  val AnyInputs: StepInputs.AnyInputs = StepInputs(state = (), params = ())

  def fromState[State](state: State): StepInputs[State, Any]      = StepInputs(state = state, params = ())
  def fromParams[Params](params: Params): StepInputs[Any, Params] = StepInputs(state = (), params = params)

  def provideParameters[P](params: P): StepInputs[Unit, P] = setParameters(params)

  def provideState[S](state: S): StepInputs[S, Unit] = setState(state)

  def setParameters[P](params: P): StepInputs[Unit, P] =
    StepInputs(params = params, state = ())

  def setState[S](state: S): StepInputs[S, Unit] =
    StepInputs(state = state, params = ())

  val unit: StepInputs[Unit, Unit]                       = StepInputs(state = (), params = ())
  val none: StepInputs[Option[Nothing], Option[Nothing]] = StepInputs(state = None, params = None)
}
