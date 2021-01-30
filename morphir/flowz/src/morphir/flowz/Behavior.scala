package morphir.flowz

import zio._
abstract class Behavior[-SIn, +SOut, -Msg, -R, +E, +A] { self =>
  protected def behavior(state: SIn)(message: Msg): ZIO[R, E, (SOut, A)]

  /**
   * Triggers the behavior, thus producing an effect.
   * An alias for run.
   */
  final def run(state: SIn, message: Msg): ZIO[R, E, (SOut, A)] =
    behavior(state)(message)

  /**
   * Triggers the behavior, thus producing an effect.
   * An alias for run.
   */
  final def trigger(state: SIn, message: Msg): ZIO[R, E, (SOut, A)] = run(state, message)

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
      protected def behavior(state: Any)(message: Msg): ZIO[R, E, (Any, A)] = f(message).map(state -> _)
    }

  /**
   * Constructs a behavior that always succeeds with the given value.
   */
  def succeed[A](value: => A): ReturnBehavior[A] = Return(value)

  /**
   * Constructs a behavior that always fails with the given error.
   */
  def fail[E](error: E): EffectBehavior[E, Nothing] = Fail(error)

  final case class Fail[E](error: E) extends EffectBehavior[E, Nothing] {
    protected def behavior(state: Any)(message: Any): ZIO[Any, E, (Any, Nothing)] = ZIO.fail(error)
  }

  final case class Return[A](value: A) extends StatelessBehavior[Any, Any, Nothing, A] {
    protected def behavior(state: Any)(message: Any): ZIO[Any, Nothing, (Any, A)] = ZIO.succeed((state, value))
  }

  /**
   * Represents a stateful behavior that is constructed from an effect.
   */
  final case class Stateful[State, StateOut, Msg, R, E, A](private val effect: ZIO[(State, Msg, R), E, (StateOut, A)])
      extends Behavior[State, StateOut, Msg, R, E, A] {
    protected def behavior(state: State)(message: Msg): ZIO[R, E, (StateOut, A)] =
      effect.provideSome[R](r => (state, message, r))
  }

  /**
   * Represents a stateless behavior that is constructed from an effect.
   */
  final case class Stateless[Msg, R, E, A](private val effect: ZIO[(Msg, R), E, A])
      extends StatelessBehavior[Msg, R, E, A] {
    protected def behavior(state: Any)(message: Msg): ZIO[R, E, (Any, A)] =
      effect.map((state, _)).provideSome[R](r => (message, r))
  }
}
