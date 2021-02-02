package morphir.flowz

import zio._

import scala.annotation.nowarn

/**
 * A behavior is a purely functional description of a computation that requires
 * an environment `R`, an initial state of `SIn` and an input/update message `Msg`.
 * It may fail with either an `E` or succeed with an updated state `SOut` and a result `A`.
 */
abstract class Behavior[-SIn, +SOut, -Msg, -R, +E, +A] { self =>
  import Behavior._

  /**
   * Maps the success value of this behavior to a constant value.
   */
  final def as[B](b: => B): Behavior[SIn, SOut, Msg, R, E, B] = map(_ => b)

  /**
   * Get this behavior as an effect.
   */
  protected final lazy val asEffect: ZIO[(SIn, Msg, R), E, BehaviorResult[SOut, A]] = toEffect

  /**
   * Defines the underlying behavior.
   */
  protected def behavior(state: SIn, message: Msg): ZIO[R, E, BehaviorResult[SOut, A]]

  // def flatMap[SOut1, Msg1 <: Msg, R1 <: R, E1 >: E, B](
  //   f: A => Behavior[SOut, SOut1, Msg1, R1, E1, B]
  // ): Behavior[SIn, SOut1, Msg1, R1, E1, B] =
  //   Behavior[SIn, SOut1, Msg1, R1, E1, B](
  //     ZIO.accessM[(SIn, Msg, R)] { case (s1, msg, r) =>
  //       asEffect.provide((s1, msg, r)).flatMap(res => f(res.result).asEffect)
  //     }
  //   )

  //def getState =

  /**
   * Returns a behavior whose success is mapped by the specified function f.
   */
  final def map[B](f: A => B): Behavior[SIn, SOut, Msg, R, E, B] = FromEffect(asEffect.map(_.map(f)))

  final def mapResults[SOut2, B](
    f: BehaviorResult[SOut, A] => BehaviorResult[SOut2, B]
  ): Behavior[SIn, SOut2, Msg, R, E, B] =
    FromEffect(asEffect.map(res => res.transform(f)))

  final def mapState[SOut2](f: SOut => SOut2): Behavior[SIn, SOut2, Msg, R, E, A] = FromEffect(
    asEffect.map(_.mapState(f))
  )

  /**
   * Provide the behavior all its required inputs and its environment
   */
  final def provide(initialState: SIn, message: Msg, environment: R): IndieBehavior[SOut, E, A] =
    Behavior(asEffect.provide((initialState, message, environment)))

  /**
   * Provide the behavior all its required inputs and its environment
   */
  final def provide(message: Msg)(implicit ev1: Any <:< SIn, ev2: Any <:< R): IndieBehavior[SOut, E, A] =
    Behavior(asEffect.provide(((), message, ())))

  /**
   * Provide the behavior all its required inputs and its environment
   */
  final def provide(initialState: SIn, message: Msg)(implicit ev1: Any <:< R): IndieBehavior[SOut, E, A] =
    Behavior(asEffect.provide((initialState, message, ())))

  /**
   * Provide the behavior with its environment.
   */
  final def provideEnvironment(environment: R): Behavior[SIn, SOut, Msg, Any, E, A] =
    Behavior(ZIO.accessM[(SIn, Msg, Any)] { case (initialState, message, _) =>
      asEffect.provide((initialState, message, environment))
    })

  /**
   * Provide the behavior with its initial state and message.
   */
  final def provideInputs(initialState: SIn, message: Msg): Behavior[Any, SOut, Any, R, E, A] =
    Behavior(ZIO.accessM[(Any, Any, R)] { case (_, _, r) => asEffect.provide((initialState, message, r)) })

  /**
   * Provide the behavior with its update message.
   */
  final def provideMessage(message: Msg): Behavior[SIn, SOut, Any, R, E, A] =
    Behavior(ZIO.accessM[(SIn, Any, R)] { case (initialState, _, r) => asEffect.provide((initialState, message, r)) })

  /**
   * Provide the behavior with its initial state
   */
  final def provideState(initialState: SIn): Behavior[Any, SOut, Msg, R, E, A] =
    Behavior(ZIO.accessM[(Any, Msg, R)] { case (_, msg, r) => asEffect.provide((initialState, msg, r)) })

  /**
   * Runs the behavior.
   */
  final def run(implicit ev1: Any <:< SIn, ev2: Any <:< Msg, ev3: Any <:< R): ZIO[R, E, BehaviorResult[SOut, A]] =
    run((), ()).provide(ev3(()))

  /**
   * Runs the behavior.
   */
  final def run(state: SIn, message: Msg): ZIO[R, E, BehaviorResult[SOut, A]] =
    behavior(state, message)

  /**
   * Runs the behavior.
   */
  final def runResult(implicit ev1: Any <:< SIn, ev2: Any <:< Msg): ZIO[R, E, A] =
    run((), ()).map(_.result)

  def toEffect: ZIO[(SIn, Msg, R), E, BehaviorResult[SOut, A]] = ZIO.accessM[(SIn, Msg, R)] { case (stateIn, msg, r) =>
    behavior(stateIn, msg).provide(r)
  }

  /**
   * Triggers the behavior, thus producing an effect.
   */
  final def trigger(state: SIn, message: Msg): ZIO[R, E, BehaviorResult[SOut, A]] = run(state, message)

}
object Behavior {
  def apply[InitialState, StateOut, Msg, R, E, A](
    effect: BehaviorEffect[InitialState, StateOut, Msg, R, E, A]
  ): Behavior[InitialState, StateOut, Msg, R, E, A] =
    FromEffect[InitialState, StateOut, Msg, R, E, A](effect)

  implicit def behaviorFromFunctionM[SIn, SOut, Msg, R, Err, A](
    f: (SIn, Msg) => ZIO[R, Err, BehaviorResult[SOut, A]]
  ): Behavior[SIn, SOut, Msg, R, Err, A] =
    FromEffect[SIn, SOut, Msg, R, Err, A](ZIO.accessM[(SIn, Msg, R)] { case (state, msg, r) =>
      f(state, msg).provide(r)
    })

  /**
   * Constructs a behavior that always fails with the given error.
   */
  def fail[E](error: E): IndieBehavior[Nothing, E, Nothing] = Fail(error)

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
   * Constructs a Behavior that gets the initial state unchanged.
   */
  def get[S]: Behavior[S, S, Any, Any, Nothing, S] =
    modify(s => (s, s))

  /**
   * Constructs a Behavior from a modify function.
   */
  def modify[S1, S2, A](f: S1 => (S2, A)): Behavior[S1, S2, Any, Any, Nothing, A] =
    Modify(f)

  def outputting[S, Value](state: S, value: Value): IndieBehavior[S, Nothing, Value] =
    Succeed(newState = state, value = value)

  /**
   * Conatructs a Behavior that sets the state to the specified value.å
   */
  def set[S](state: S): Behavior[Any, S, Any, Any, Nothing, Unit] = modify(_ => (state, ()))

  /**
   * Constructs a stateless behavior.
   */
  def stateless[Msg, R, E, A](f: Msg => ZIO[R, E, A]): StatelessBehavior[Msg, R, E, A] =
    new StatelessBehavior[Msg, R, E, A] {
      protected def behavior(state: Any, message: Msg): ZIO[R, E, BehaviorResult[Any, A]] = f(message).map(state -> _)
    }

  /**
   * Constructs a behavior that always succeeds with the given value.
   */
  def succeed[S, A](value: => A): ReturnBehavior[S, A] = Return(value)

  def unit[S]: Behavior[S, S, Any, Any, Nothing, Unit] =
    succeed(())

  /**
   * Constructs a behavior from a function that produces a new state and a result given an initial state and a message.
   */
  def update[S1, S2, Msg, A](f: (S1, Msg) => (S2, A)): Behavior[S1, S2, Msg, Any, Nothing, A] =
    Stateful(ZIO.access[(S1, Msg, Any)] { case (s1, msg, _) =>
      val (s2, a) = f(s1, msg)
      BehaviorResult(s2, a)
    })

  final case class Fail[E](error: E) extends IndieBehavior[Nothing, E, Nothing] {
    protected def behavior(state: Any, message: Any): ZIO[Any, E, BehaviorResult[Nothing, Nothing]] = ZIO.fail(error)
  }

  final case class FromEffect[StateIn, StateOut, Msg, Env, E, A](
    zio: ZIO[(StateIn, Msg, Env), E, BehaviorResult[StateOut, A]]
  ) extends Behavior[StateIn, StateOut, Msg, Env, E, A] {
    override lazy val toEffect: ZIO[(StateIn, Msg, Env), E, BehaviorResult[StateOut, A]] = zio
    protected def behavior(state: StateIn, message: Msg): ZIO[Env, E, BehaviorResult[StateOut, A]] =
      zio.provideSome[Env](env => (state, message, env))
  }

  final case class Modify[-S1, +S2, +A](func: S1 => (S2, A)) extends Behavior[S1, S2, Any, Any, Nothing, A] {

    protected def behavior(state: S1, message: Any): ZIO[Any, Nothing, BehaviorResult[S2, A]] =
      ZIO.effectTotal(func(state))

  }

  // final case class FromUpdateFunction[State, Msg, R, Err, Model](
  //   updateFunc: (State, Msg) => Behavior[State, State, Msg, R, Err, Model]
  // ) extends Behavior[State, State, Msg, R, Err, Model] {
  //   protected def behavior(state: State, message: Msg): ZIO[R, Err, BehaviorResult[State, Model]] =
  //     updateFunc(state, message).behavior(state, message)

  // }

  final case class Succeed[S, A](newState: S, value: A) extends IndieBehavior[S, Nothing, A] {
    protected def behavior(state: Any, message: Any): ZIO[Any, Nothing, BehaviorResult[S, A]] =
      ZIO.succeed((newState, value))
  }

  final case class Return[S, A](value: A) extends Behavior[S, S, Any, Any, Nothing, A] {
    protected def behavior(state: S, message: Any): ZIO[Any, Nothing, BehaviorResult[S, A]] =
      ZIO.succeed((state, value))
  }

  /**
   * Represents a stateful behavior that is constructed from an effect.
   */
  final case class Stateful[S, StateOut, Msg, R, E, A](
    private val effect: BehaviorEffect[S, StateOut, Msg, R, E, A]
  ) extends Behavior[S, StateOut, Msg, R, E, A] {
    protected def behavior(state: S, message: Msg): ZIO[R, E, BehaviorResult[StateOut, A]] =
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
