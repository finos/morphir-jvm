package morphir.flowz

import zio._

/**
 * A behavior is a purely functional description of a computation that requires
 * an environment `R`, an initial state of `SIn` and an input/update message `Msg`.
 * It may fail with either an `E` or succeed with an updated state `SOut` and a result `A`.
 */
abstract class Behavior[-SIn, +SOut, -Msg, -R, +E, +A] { self =>
  import Behavior._

  /**
   * Connect this Behavior to the given Behavior by connecting the output state of this Behavior to the input state
   * of the other Behavior and by connecting the result of this Behavior to the input of the other.
   */
  def >>>[SOut2, R1 <: R, E1 >: E, B](that: Behavior[SOut, SOut2, A, R1, E1, B]): Behavior[SIn, SOut2, Msg, R1, E1, B] =
    self andThen that

  /**
   * A variant of flatMap that ignores the values (result and state)  produced by this behavior.
   */
  def *>[SIn1 <: SIn, Msg1 <: Msg, R1 <: R, E1 >: E, SOut1, B](
    that: Behavior[SIn1, SOut1, Msg1, R1, E1, B]
  ): Behavior[SIn1, SOut1, Msg1, R1, E1, B] =
    Behavior(self.asEffect *> that.asEffect)

  /**
   * An operator alias for zip.
   */
  def <*>[SIn1 <: SIn, Msg1 <: Msg, R1 <: R, E1 >: E, SOut1, B](
    that: Behavior[SIn1, SOut1, Msg1, R1, E1, B]
  ): Behavior[SIn1, (SOut, SOut1), Msg1, R1, E1, (A, B)] = self zip that

  /**
   * An operator alias for zipPar.
   */
  def |+|[SIn1 <: SIn, Msg1 <: Msg, R1 <: R, E1 >: E, SOut1, B](
    that: Behavior[SIn1, SOut1, Msg1, R1, E1, B]
  ): Behavior[SIn1, (SOut, SOut1), Msg1, R1, E1, (A, B)] = self zipPar that

  /**
   * Sequences the specified behavior after this behavior, but ignores the
   * values (result and state) produced by the behavior.
   */
  def <*[SIn1 <: SIn, Msg1 <: Msg, R1 <: R, E1 >: E, SOut1, B](
    that: Behavior[SIn1, SOut1, Msg1, R1, E1, B]
  ): Behavior[SIn1, SOut, Msg1, R1, E1, A] = Behavior(self.asEffect <* that.asEffect)

  /**
   * Executes the given behavior upon the successful execution of this behavior.
   */
  def andThen[SOut2, R1 <: R, E1 >: E, B](
    that: Behavior[SOut, SOut2, A, R1, E1, B]
  ): Behavior[SIn, SOut2, Msg, R1, E1, B] =
    Behavior(ZIO.accessM[(SIn, Msg, R1)] { case (_, _, r) =>
      self.asEffect.flatMap(success => that.asEffect.provide((success.state, success.result, r)))
    })

  /**
   * Maps the success value of this behavior to a constant value.
   */
  final def as[B](b: => B): Behavior[SIn, SOut, Msg, R, E, B] = map(_ => b)

  /**
   * Get this behavior as an effect.
   */
  protected final lazy val asEffect: BehaviorEffect[SIn, SOut, Msg, R, E, A] = toEffect

  /**
   * Defines the underlying behavior.
   */
  protected def behavior(state: SIn, message: Msg): ZIO[R, E, BehaviorSuccess[SOut, A]]

  def flatMap[SOut1, Msg1 <: Msg, R1 <: R, E1 >: E, B](
    f: A => Behavior[SOut, SOut1, Msg1, R1, E1, B]
  ): Behavior[SIn, SOut1, Msg1, R1, E1, B] =
    Behavior[SIn, SOut1, Msg1, R1, E1, B](
      ZIO.accessM[(SIn, Msg1, R1)] { case (_, msg, r) =>
        asEffect.flatMap { res: BehaviorSuccess[SOut, A] =>
          f(res.result).asEffect.provide((res.state, msg, r))
        }
      }
    )

  //def getState =

  /**
   * Returns a behavior whose success is mapped by the specified function f.
   */
  final def map[B](f: A => B): Behavior[SIn, SOut, Msg, R, E, B] = FromEffect(asEffect.map(_.map(f)))

  final def mapResults[SOut2, B](
    f: BehaviorSuccess[SOut, A] => BehaviorSuccess[SOut2, B]
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
    Behavior(asEffect.provideState(initialState))

  /**
   * Runs the behavior.
   */
  final def run(implicit ev1: Any <:< SIn, ev2: Any <:< Msg, ev3: Any <:< R): ZIO[R, E, BehaviorSuccess[SOut, A]] =
    run((), ()).provide(ev3(()))

  /**
   * Runs the behavior.
   */
  final def run(state: SIn, message: Msg): ZIO[R, E, BehaviorSuccess[SOut, A]] =
    behavior(state, message)

  /**
   * Runs the behavior.
   */
  final def runResult(implicit ev1: Any <:< SIn, ev2: Any <:< Msg): ZIO[R, E, A] =
    run((), ()).map(_.result)

  /**
   * Returns a behavior that effectfully "peeks" at the success of this behavior.
   *
   * {{{
   * readFile("data.json").tap(putStrLn)
   * }}}
   */
  def tap[R1 <: R, E1 >: E](
    func: BehaviorSuccess[SOut, A] => ZIO[R1, E1, Any]
  ): Behavior[SIn, SOut, Msg, R1, E1, A] =
    Behavior[SIn, SOut, Msg, R1, E1, A](
      ZIO.accessM[(SIn, Msg, R1)] { case (_, _, r1) =>
        self.asEffect.tap(success => func(success).provide(r1))
      }
    )

  def toEffect: ZIO[(SIn, Msg, R), E, BehaviorSuccess[SOut, A]] = ZIO.accessM[(SIn, Msg, R)] { case (stateIn, msg, r) =>
    behavior(stateIn, msg).provide(r)
  }

  /**
   * Triggers the behavior, thus producing an effect.
   */
  final def trigger(state: SIn, message: Msg): ZIO[R, E, BehaviorSuccess[SOut, A]] = run(state, message)

  final def trigger(message: Msg)(implicit ev: Any <:< SIn): ZIO[R, E, BehaviorSuccess[SOut, A]] = run(state, message)

  def zip[SIn1 <: SIn, Msg1 <: Msg, R1 <: R, E1 >: E, SOut1, B](
    that: Behavior[SIn1, SOut1, Msg1, R1, E1, B]
  ): Behavior[SIn1, (SOut, SOut1), Msg1, R1, E1, (A, B)] =
    Behavior((self.asEffect zip that.asEffect) map { case (left, right) => left zip right })

  def zipPar[SIn1 <: SIn, Msg1 <: Msg, R1 <: R, E1 >: E, SOut1, B](
    that: Behavior[SIn1, SOut1, Msg1, R1, E1, B]
  ): Behavior[SIn1, (SOut, SOut1), Msg1, R1, E1, (A, B)] =
    Behavior((self.asEffect zipPar that.asEffect) map { case (left, right) => left zip right })

  def zipWith[SIn1 <: SIn, Msg1 <: Msg, R1 <: R, E1 >: E, SOut1, B, S, C](that: Behavior[SIn1, SOut1, Msg1, R1, E1, B])(
    f: (BehaviorSuccess[SOut, A], BehaviorSuccess[SOut1, B]) => BehaviorSuccess[S, C]
  ): Behavior[SIn1, S, Msg1, R1, E1, C] =
    Behavior((self.asEffect zipWith that.asEffect)(f))

  def zipWithPar[SIn1 <: SIn, Msg1 <: Msg, R1 <: R, E1 >: E, SOut1, B, S, C](
    that: Behavior[SIn1, SOut1, Msg1, R1, E1, B]
  )(
    f: (BehaviorSuccess[SOut, A], BehaviorSuccess[SOut1, B]) => BehaviorSuccess[S, C]
  ): Behavior[SIn1, S, Msg1, R1, E1, C] =
    Behavior((self.asEffect zipWithPar that.asEffect)(f))
}
object Behavior extends BehaviorEffectSyntax {
  def apply[InitialState, StateOut, Msg, R, E, A](
    effect: BehaviorEffect[InitialState, StateOut, Msg, R, E, A]
  ): Behavior[InitialState, StateOut, Msg, R, E, A] =
    FromEffect[InitialState, StateOut, Msg, R, E, A](effect)

  implicit def behaviorFromFunctionM[SIn, SOut, Msg, R, Err, A](
    f: (SIn, Msg) => ZIO[R, Err, BehaviorSuccess[SOut, A]]
  ): Behavior[SIn, SOut, Msg, R, Err, A] =
    FromEffect[SIn, SOut, Msg, R, Err, A](ZIO.accessM[(SIn, Msg, R)] { case (state, msg, r) =>
      f(state, msg).provide(r)
    })

  /**
   * Constructs a behavior that always fails with the given error.
   */
  def fail[E](error: E): IndieBehavior[Nothing, E, Nothing] = Fail(error)

  def fromEffect[SIn, SOut, Msg, R, E, A](
    effect: ZIO[(SIn, Msg, R), E, BehaviorSuccess[SOut, A]]
  )(evState: NeedsInputState[SIn], evMsg: NeedsMsg[Msg]): Behavior[SIn, SOut, Msg, R, E, A] = {
    val _ = (evState, evMsg) //NOTE: Suppresses the warning about these not being used
    new Behavior[SIn, SOut, Msg, R, E, A] {
      protected def behavior(state: SIn, message: Msg): ZIO[R, E, BehaviorSuccess[SOut, A]] =
        effect.provideSome[R](r => (state, message, r))
    }
  }

  def fromEffect[S, R, E, A](effect: ZIO[R, E, BehaviorSuccess[S, A]]): Behavior[Any, S, Any, R, E, A] =
    FromEffect(ZIO.accessM[(Any, Any, R)] { case (_, _, r) => effect.provide(r) })

  /**
   * Create a behavior by providing a possibly impure function.
   *
   * For example:
   *
   * {{{
   *   val intConverter = Behavior.fromFunction { numberStr:String =>
   *      numberStr.toInt
   *   }
   * }}}
   */
  def fromFunction[In, Out](f: In => Out): FuncBehavior[In, Out] =
    MessageHandler(ZIO.accessM[In](input => ZIO.effect(f(input))))

  /**
   * Constructs a Behavior that gets the initial state unchanged.
   */
  def get[S]: Behavior[S, S, Any, Any, Nothing, S] =
    modify(s => (s, s))

  def getMessage[Msg]: Behavior[Any, Any, Msg, Any, Nothing, Msg] =
    MessageHandler(ZIO.access[Msg](identity))

  /**
   * Constructs a Behavior from a modify function.
   */
  def modify[S1, S2, A](f: S1 => (S2, A)): Behavior[S1, S2, Any, Any, Nothing, A] =
    Modify(f)

  def outputting[S, Value](state: S, value: Value): IndieBehavior[S, Nothing, Value] =
    SetOutputs(newState = state, value = value)

  /**
   * Constructs a Behavior that sets the state to the specified value.å
   */
  def set[S](state: S): Behavior[Any, S, Any, Any, Nothing, Unit] = modify(_ => (state, ()))

  /**
   * Constructs a stateless behavior from a function that produces an effect.
   */
  def stateless[Msg, R, E, A](f: Msg => ZIO[R, E, A]): StatelessBehavior[Msg, R, E, A] =
    new AbstractStatelessBehavior[Msg, R, E, A] {
      protected def behavior(state: Any, message: Msg): ZIO[R, E, BehaviorSuccess[Any, A]] = f(message).map(state -> _)
    }

  /**
   * Constructs a behavior that always succeeds with the given value.
   */
  def succeed[S, A](value: => A): Behavior[Any, Any, Any, Any, Nothing, A] = Succeed(value)

  val unit: Behavior[Any, Any, Any, Any, Nothing, Unit] =
    succeed(())

  /**
   * Constructs a behavior from a function that produces a new state and a result given an initial state and a message.
   * This function is expected to be pure without side effects and should not throw any exceptions.
   */
  def update[S1, S2, Msg, A](f: (S1, Msg) => (S2, A)): Behavior[S1, S2, Msg, Any, Nothing, A] =
    Stateful(ZIO.access[(S1, Msg, Any)] { case (s1, msg, _) =>
      val (s2, a) = f(s1, msg)
      BehaviorSuccess(s2, a)
    })

  final case class Fail[E](error: E) extends IndieBehavior[Nothing, E, Nothing] {
    protected def behavior(state: Any, message: Any): ZIO[Any, E, BehaviorSuccess[Nothing, Nothing]] = ZIO.fail(error)
  }

  final case class FromEffect[StateIn, StateOut, Msg, Env, E, A](
    zio: ZIO[(StateIn, Msg, Env), E, BehaviorSuccess[StateOut, A]]
  ) extends Behavior[StateIn, StateOut, Msg, Env, E, A] {
    override lazy val toEffect: ZIO[(StateIn, Msg, Env), E, BehaviorSuccess[StateOut, A]] = zio
    protected def behavior(state: StateIn, message: Msg): ZIO[Env, E, BehaviorSuccess[StateOut, A]] =
      zio.provideSome[Env](env => (state, message, env))
  }

  final case class MessageHandler[-Msg, +Err, +A](private val zio: ZIO[Msg, Err, A])
      extends AbstractStatelessBehavior[Msg, Any, Err, A] {

    protected def behavior(state: Any, message: Msg): ZIO[Any, Err, BehaviorSuccess[Any, A]] =
      zio.map(result => BehaviorSuccess(state = state, result = result)).provide(message)

  }

  final case class Modify[-S1, +S2, +A](func: S1 => (S2, A)) extends Behavior[S1, S2, Any, Any, Nothing, A] {

    protected def behavior(state: S1, message: Any): ZIO[Any, Nothing, BehaviorSuccess[S2, A]] =
      ZIO.effectTotal(func(state))

  }

  final case class SetOutputs[S, A](newState: S, value: A) extends IndieBehavior[S, Nothing, A] {
    protected def behavior(state: Any, message: Any): ZIO[Any, Nothing, BehaviorSuccess[S, A]] =
      ZIO.succeed((newState, value))
  }

  final case class Succeed[+A](value: A) extends Behavior[Any, Any, Any, Any, Nothing, A] {
    protected def behavior(state: Any, message: Any): ZIO[Any, Nothing, BehaviorSuccess[Any, A]] =
      ZIO.succeed(BehaviorSuccess(state, value))
  }

  /**
   * Represents a stateful behavior that is constructed from an effect.
   */
  final case class Stateful[S, StateOut, Msg, R, E, A](
    private val effect: BehaviorEffect[S, StateOut, Msg, R, E, A]
  ) extends Behavior[S, StateOut, Msg, R, E, A] {
    protected def behavior(state: S, message: Msg): ZIO[R, E, BehaviorSuccess[StateOut, A]] =
      effect.provideSome[R](r => (state, message, r))
  }

  /**
   * Represents a stateless behavior that is constructed from an effect.
   */
  final case class Stateless[Msg, R, E, A](private val effect: ZIO[(Msg, R), E, A])
      extends AbstractStatelessBehavior[Msg, R, E, A] {
    protected def behavior(state: Any, message: Msg): ZIO[R, E, BehaviorSuccess[Any, A]] =
      effect.map(value => BehaviorSuccess(state, value)).provideSome[R](r => (message, r))
  }
}
