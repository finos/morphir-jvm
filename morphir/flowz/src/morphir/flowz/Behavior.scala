package morphir.flowz

import zio._

import scala.annotation.nowarn

abstract class Behavior[-SIn, +SOut, -Msg, -R, +E, +A] { self =>
  protected def behavior(state: SIn, message: Msg): ZIO[R, E, BehaviorResult[SOut, A]]

  /**
   * Runs the behavior.
   */
  final def run(state: SIn, message: Msg): ZIO[R, E, BehaviorResult[SOut, A]] =
    behavior(state, message)

  /**
   * Triggers the behavior, thus producing an effect.
   */
  final def trigger(state: SIn, message: Msg): ZIO[R, E, BehaviorResult[SOut, A]] = run(state, message)

}
object Behavior {
  def apply[State, StateOut, Msg, R, E, A](
    f: (State, Msg) => ZIO[R, E, (StateOut, A)]
  ): Behavior[State, StateOut, Msg, R, E, A] =
    Stateful[State, StateOut, Msg, R, E, A](ZIO.accessM[(State, Msg, R)] { case (state, msg, r) =>
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
    f: (SIn, Msg) => ZIO[R, Err, (SOut, A)]
  ): Behavior[SIn, SOut, Msg, R, Err, A] =
    Behavior[SIn, SOut, Msg, R, Err, A](f)

  def fromEffect[SIn, SOut, Msg, R, E, A](
    effect: ZIO[(SIn, Msg, R), E, (SOut, A)]
  )(@nowarn evState: NeedsInputState[SIn], @nowarn evMsg: NeedsMsg[Msg]): Behavior[SIn, SOut, Msg, R, E, A] =
    new Behavior[SIn, SOut, Msg, R, E, A] {
      protected def behavior(state: SIn, message: Msg): ZIO[R, E, BehaviorResult[SOut, A]] =
        effect.provideSome[R](r => (state, message, r)).map(BehaviorResult.fromPair)
    }

  def fromEffect[S, R, E, A](effect: ZIO[R, E, (S, A)]): Behavior[Any, S, Any, R, E, A] =
    new Behavior[Any, S, Any, R, E, A] {
      protected def behavior(state: Any, message: Any): ZIO[R, E, BehaviorResult[S, A]] =
        effect.map(BehaviorResult.fromPair)
    }

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
  final case class Stateful[State, StateOut, Msg, R, E, A](private val effect: ZIO[(State, Msg, R), E, (StateOut, A)])
      extends Behavior[State, StateOut, Msg, R, E, A] {
    protected def behavior(state: State, message: Msg): ZIO[R, E, BehaviorResult[StateOut, A]] =
      effect.map(BehaviorResult.fromPair).provideSome[R](r => (state, message, r))
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
