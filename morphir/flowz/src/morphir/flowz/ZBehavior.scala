package morphir.flowz

import zio.{ CanFail, NeedsEnv, ZIO }

object ZBehavior {

  def apply[InitialState, StateOut, Msg, Env, E, A](
    func: (InitialState, Msg) => ZIO[Env, E, StepSuccess[StateOut, A]]
  )(implicit
    evStateIn: NeedsInputState[InitialState],
    evMsg: NeedsMsg[Msg],
    evEnv: NeedsEnv[Env],
    evCanFail: CanFail[E]
  ): ZBehavior[InitialState, StateOut, Msg, Env, E, A] = {
    val _ = (evStateIn, evMsg, evCanFail)
    ZIO.accessM[(InitialState, Msg, Env)] { case (stateIn, msg, env) => func(stateIn, msg).provide(env) }
  }

  implicit def effectFromFunc[InitialState, StateOut, In, Env, E, A](
    func: (InitialState, In) => ZIO[Env, E, StepSuccess[StateOut, A]]
  )(implicit
    evStateIn: NeedsInputState[InitialState],
    evMsg: NeedsMsg[In],
    evEnv: NeedsEnv[Env],
    evCanFail: CanFail[E]
  ): ZBehavior[InitialState, StateOut, In, Env, E, A] = apply(func)

  implicit def effectFromFunc2[InitialState, StateOut, In, Env, E, A](
    func: (InitialState, In) => ZIO[Env, E, (StateOut, A)]
  )(implicit
    evStateIn: NeedsInputState[InitialState],
    evMsg: NeedsMsg[In],
    evEnv: NeedsEnv[Env],
    evCanFail: CanFail[E]
  ): ZBehavior[InitialState, StateOut, In, Env, E, A] = {
    val _ = (evStateIn, evMsg, evCanFail)
    ZIO.accessM[(InitialState, In, Env)] { case (stateIn, msg, env) =>
      func(stateIn, msg).map(StepSuccess.fromPair).provide(env)
    }
  }

}
