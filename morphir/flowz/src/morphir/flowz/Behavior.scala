package morphir.flowz

import zio._

import scala.annotation.nowarn

abstract class Behavior[-SIn, +SOut, -Msg, -R, +E, +A] { self =>

  final lazy val asEffect: ZIO[(SIn, Msg, R), E, BehaviorResult[SOut, A]] = toEffect

  def map[B](f: A => B): Behavior[SIn, SOut, Msg, R, E, B] = ???

  protected def behavior(state: SIn, message: Msg): ZIO[R, E, BehaviorResult[SOut, A]]

  /**
   * Runs the behavior.
   */
  final def run(state: SIn, message: Msg): ZIO[R, E, BehaviorResult[SOut, A]] =
    behavior(state, message)

  def toEffect: ZIO[(SIn, Msg, R), E, BehaviorResult[SOut, A]] = ZIO.accessM[(SIn, Msg, R)] { case (stateIn, msg, r) =>
    behavior(stateIn, msg).provide(r)
  }

  /**
   * Triggers the behavior, thus producing an effect.
   */
  final def trigger(state: SIn, message: Msg): ZIO[R, E, BehaviorResult[SOut, A]] = run(state, message)

}
object Behavior {
  def apply[State, StateOut, Msg, R, E, A](
    f: (State, Msg) => ZIO[R, E, BehaviorResult[StateOut, A]]
  ): Behavior[State, StateOut, Msg, R, E, A] =
    FromEffect[State, StateOut, Msg, R, E, A](ZIO.accessM[(State, Msg, R)] { case (state, msg, r) =>
      f(state, msg).provide(r)
    })

  /**
   * Constructs a stateless behavior.
   */
  def stateless[Msg, R, E, A](f: Msg => ZIO[R, E, A]): StatelessBehavior[Msg, R, E, A] =
    new StatelessBehavior[Msg, R, E, A] {
      protected def behavior(state: Any, message: Msg): ZIO[R, E, BehaviorResult[Any, A]] = f(message).map(state -> _)
    }

  implicit def behaviorFromFunctionM[SIn, SOut, Msg, R, Err, A](
    f: (SIn, Msg) => ZIO[R, Err, BehaviorResult[SOut, A]]
  ): Behavior[SIn, SOut, Msg, R, Err, A] =
    Behavior[SIn, SOut, Msg, R, Err, A](f)

  def fromEffect[SIn, SOut, Msg, R, E, A](
    effect: ZIO[(SIn, Msg, R), E, BehaviorResult[SOut, A]]
  )(@nowarn evState: NeedsInputState[SIn], @nowarn evMsg: NeedsMsg[Msg]): Behavior[SIn, SOut, Msg, R, E, A] =
    new Behavior[SIn, SOut, Msg, R, E, A] {
      protected def behavior(state: SIn, message: Msg): ZIO[R, E, BehaviorResult[SOut, A]] =
        effect.provideSome[R](r => (state, message, r))
    }

  def fromEffect[S, R, E, A](effect: ZIO[R, E, BehaviorResult[S, A]]): Behavior[Any, S, Any, R, E, A] =
    FromEffect(ZIO.accessM[(Any, Any, R)] { case (_, _, r) => effect.provide(r) })

  /**
   * Constructs a behavior that always fails with the given error.
   */
  def fail[E](error: E): EffectBehavior[Nothing, E, Nothing] = Fail(error)

  def outputting[State, Value](state: State, value: Value): EffectBehavior[State, Nothing, Value] =
    Succeed(newState = state, value = value)

  /**
   * Constructs a behavior that always succeeds with the given value.
   */
  def succeed[A](value: => A): ReturnBehavior[A] = Return(value)

  final case class Fail[E](error: E) extends EffectBehavior[Nothing, E, Nothing] {
    protected def behavior(state: Any, message: Any): ZIO[Any, E, BehaviorResult[Nothing, Nothing]] = ZIO.fail(error)
  }

  final case class FromEffect[StateIn, StateOut, Msg, Env, E, A](
    zio: ZIO[(StateIn, Msg, Env), E, BehaviorResult[StateOut, A]]
  ) extends Behavior[StateIn, StateOut, Msg, Env, E, A] {
    override lazy val toEffect: ZIO[(StateIn, Msg, Env), E, BehaviorResult[StateOut, A]] = zio
    protected def behavior(state: StateIn, message: Msg): ZIO[Env, E, BehaviorResult[StateOut, A]] =
      zio.provideSome[Env](env => (state, message, env))
  }

  final case class Succeed[S, A](newState: S, value: A) extends EffectBehavior[S, Nothing, A] {
    protected def behavior(state: Any, message: Any): ZIO[Any, Nothing, BehaviorResult[S, A]] =
      ZIO.succeed((newState, value))
  }

  final case class Return[A](value: A) extends StatelessBehavior[Any, Any, Nothing, A] {
    protected def behavior(state: Any, message: Any): ZIO[Any, Nothing, BehaviorResult[Any, A]] =
      ZIO.succeed((state, value))
  }

  /**
   * Represents a stateful behavior that is constructed from an effect.
   */
  final case class Stateful[State, StateOut, Msg, R, E, A](
    private val effect: BehaviorEffect[State, StateOut, Msg, R, E, A]
  ) extends Behavior[State, StateOut, Msg, R, E, A] {
    protected def behavior(state: State, message: Msg): ZIO[R, E, BehaviorResult[StateOut, A]] =
      effect.provideSome[R](r => (state, message, r))
  }

  /**
   * Represents a stateless behavior that is constructed from an effect.
   */
  final case class Stateless[Msg, R, E, A](private val effect: ZIO[(Msg, R), E, A])
      extends StatelessBehavior[Msg, R, E, A] {
    protected def behavior(state: Any, message: Msg): ZIO[R, E, BehaviorResult[Any, A]] =
      effect.map(value => BehaviorResult(state, value)).provideSome[R](r => (message, r))
  }
}
