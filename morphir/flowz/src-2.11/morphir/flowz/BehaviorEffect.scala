package morphir.flowz

import zio._

import scala.annotation.nowarn

object BehaviorEffect {

  def apply[StateIn, StateOut, Msg, Env, E, A](
    func: (StateIn, Msg) => ZIO[Env, E, BehaviorResult[StateOut, A]]
  )(implicit
    @nowarn evStateIn: NeedsInputState[StateIn],
    @nowarn evMsg: NeedsMsg[Msg],
    evEnv: NeedsEnv[Env],
    @nowarn evCanFail: CanFail[E]
  ): BehaviorEffect[StateIn, StateOut, Msg, Env, E, A] =
    ZIO.accessM[(StateIn, Msg, Env)] { case (stateIn, msg, env) => func(stateIn, msg).provide(env) }

  implicit def effectFromFunc[StateIn, StateOut, Msg, Env, E, A](
    func: (StateIn, Msg) => ZIO[Env, E, BehaviorResult[StateOut, A]]
  )(implicit
    @nowarn evStateIn: NeedsInputState[StateIn],
    @nowarn evMsg: NeedsMsg[Msg],
    evEnv: NeedsEnv[Env],
    @nowarn evCanFail: CanFail[E]
  ): BehaviorEffect[StateIn, StateOut, Msg, Env, E, A] = apply(func)

  implicit def effectFromFunc2[StateIn, StateOut, Msg, Env, E, A](
    func: (StateIn, Msg) => ZIO[Env, E, (StateOut, A)]
  )(implicit
    @nowarn evStateIn: NeedsInputState[StateIn],
    @nowarn evMsg: NeedsMsg[Msg],
    evEnv: NeedsEnv[Env],
    @nowarn evCanFail: CanFail[E]
  ): BehaviorEffect[StateIn, StateOut, Msg, Env, E, A] =
    ZIO.accessM[(StateIn, Msg, Env)] { case (stateIn, msg, env) =>
      func(stateIn, msg).map(BehaviorResult.fromPair).provide(env)
    }

}
