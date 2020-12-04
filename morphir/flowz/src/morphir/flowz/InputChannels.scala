package morphir.flowz

final case class InputChannels[+State, +Params](state: State, params: Params) { self =>

  def acceptingAny: InputChannels[Any, Any] = self

  def acceptingAnyState: InputChannels[Any, Params] = self

  def acceptingAnyParams: InputChannels[State, Any] = self

  /**
   * Converts the `InputChannels` to `OutputChannels`
   */
  def toOutputs: OutputChannels[State, Params] =
    OutputChannels(state = self.state, value = self.params)

  def map[P](func: Params => P): InputChannels[State, P] =
    copy(params = func(self.params))

  def mapState[S](func: State => S): InputChannels[S, Params] =
    copy(state = func(self.state))

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
