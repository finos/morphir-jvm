package morphir.flowz

import zio._

final case class BehaviorEffect[-SIn, +SOut, -Msg, -Env, +E, +A](
  effect: ZIO[(SIn, Msg, Env), E, BehaviorResult[SOut, A]]
) extends AnyVal {}

object BehaviorEffect {

  implicit def effectFromFunc[StateIn, StateOut, Msg, Env, E, A](
    func: (StateIn, Msg) => ZIO[Env, E, BehaviorResult[StateOut, A]]
  )(implicit
    evStateIn: NeedsInputState[StateIn],
    evMsg: NeedsMsg[Msg],
    evEnv: NeedsEnv[Env]
  ): BehaviorEffect[StateIn, StateOut, Msg, Env, E, A] =
    BehaviorEffect[StateIn, StateOut, Msg, Env, E, A](
      ZIO.accessM[(StateIn, Msg, Env)] { case (stateIn, msg, env) => func(stateIn, msg).provide(env) }
    )

  implicit def effectFromFunc2[StateIn, StateOut, Msg, Env, E, A](
    func: (StateIn, Msg) => ZIO[Env, E, (StateOut, A)]
  )(implicit
    evStateIn: NeedsInputState[StateIn],
    evMsg: NeedsMsg[Msg],
    evEnv: NeedsEnv[Env]
  ): BehaviorEffect[StateIn, StateOut, Msg, Env, E, A] =
    BehaviorEffect[StateIn, StateOut, Msg, Env, E, A](
      ZIO.accessM[(StateIn, Msg, Env)] { case (stateIn, msg, env) =>
        func(stateIn, msg).map(BehaviorResult.fromPair).provide(env)
      }
    )

  implicit def toEffect[StateIn, StateOut, Msg, Env, E, A](
    effect: BehaviorEffect[StateIn, StateOut, Msg, Env, E, A]
  ): ZIO[(StateIn, Msg, Env), E, BehaviorResult[StateOut, A]] =
    effect.effect
}
