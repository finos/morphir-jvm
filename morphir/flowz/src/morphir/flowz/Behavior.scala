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
   * Returns a behavior that executes both this behavior and the specified behavior,
   * in parallel, returning the result of the provided behavior. If either side fails,
   * then the other side will be interrupted, interrupting the result.
   */
  def &>[SIn1 <: SIn, In1 <: Msg, R1 <: R, E1 >: E, SOut1, B, S, C](
    that: Behavior[SIn1, SOut1, In1, R1, E1, B]
  ): Behavior[SIn1, SOut1, In1, R1, E1, B] =
    self.zipWithPar(that)((_, b) => b)

  /**
   * Connect this Behavior to the given Behavior by connecting the output state of this Behavior to the input state
   * of the other Behavior and by connecting the result of this Behavior to the input of the other.
   */
  def >>>[SOut2, R1 <: R, E1 >: E, B](
    that: Behavior[SOut, SOut2, A, R1, E1, B]
  ): Behavior[SIn, SOut2, Msg, R1, E1, B] =
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
   * Returns a behavior that executes both this behavior and the specified behavior,
   * in parallel, this behavior's result is returned. If either side fails,
   * then the other side will be interrupted, thus interrupting the result.
   */
  def <&[SIn1 <: SIn, In1 <: Msg, R1 <: R, E1 >: E, SOut1, B, S, C](
    that: Behavior[SIn1, SOut1, In1, R1, E1, B]
  ): Behavior[SIn1, SOut, In1, R1, E1, A] =
    self.zipWithPar(that)((a, _) => a)

  /**
   * An operator alias for zip.
   */
  def <&>[SIn1 <: SIn, Msg1 <: Msg, R1 <: R, E1 >: E, SOut1, B](
    that: Behavior[SIn1, SOut1, Msg1, R1, E1, B]
  ): Behavior[SIn1, (SOut, SOut1), Msg1, R1, E1, (A, B)] = self zipPar that

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
  def <*[SIn1 <: SIn, In1 <: Msg, R1 <: R, E1 >: E, SOut1, B](
    that: Behavior[SIn1, SOut1, In1, R1, E1, B]
  ): Behavior[SIn1, SOut, In1, R1, E1, A] = Behavior(self.asEffect <* that.asEffect)

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

  /**
   * Returns a behavior whose failure and success channels have been mapped by the specified pair of
   * functions, f and g.
   */
  def bimap[E2, B](f: E => E2, g: A => B)(implicit ev: CanFail[E]): Behavior[SIn, SOut, Msg, R, E2, B] =
    Behavior(ZIO.accessM[(SIn, Msg, R)] { env =>
      self.asEffect.provide(env).bimap(f, _.map(g))
    })

  /**
   * Returns a behavior whose failure and success have been lifted into an
   * `Either`. The resulting computation cannot fail, because the failure case
   * has been exposed as part of the `Either` success case.
   */
  def either[S >: SOut <: SIn](implicit ev: CanFail[E]): Behavior[S, S, Msg, R, Nothing, Either[E, A]] =
    foldM(
      failure = e => modify(s => (s, Left(e))),
      success = a => modify(s => (s, Right(a)))
    )

  /**
   * Returns a behavior that models the execution of this behavior, followed by
   * the passing of its value to the specified continuation function `k`,
   * followed by the behavior that it returns.
   *
   * {{{
   * val parsed = readFile("foo.txt").flatMap(file => parseFile(file))
   * }}}
   */
  def flatMap[SOut1, In1 <: Msg, R1 <: R, E1 >: E, B](
    k: A => Behavior[SOut, SOut1, In1, R1, E1, B]
  ): Behavior[SIn, SOut1, In1, R1, E1, B] =
    Behavior[SIn, SOut1, In1, R1, E1, B](
      ZIO.accessM[(SIn, In1, R1)] { case (_, msg, r) =>
        asEffect.flatMap { res: BehaviorSuccess[SOut, A] =>
          k(res.result).asEffect.provide((res.state, msg, r))
        }
      }
    )

  /**
   * Folds over the failed or successful results of this behavior to yield
   * a behavior that does not fail, but succeeds with the value of the left
   * or right function passed to `fold`.
   */
  def fold[S >: SOut <: SIn, B](failure: E => B, success: A => B)(implicit
    ev: CanFail[E]
  ): Behavior[S, S, Msg, R, Nothing, B] =
    self.foldM(e => modify(s => (s, failure(e): B)), a => modify(s => (s, success(a))))

  /**
   * A more powerful version of `foldM` that allows recovering from any kind of failure except interruptions.
   */
  def foldCauseM[SIn0 <: SIn, SOut1, Msg1 <: Msg, R1 <: R, E1, B](
    failure: Cause[E] => Behavior[SIn0, SOut1, Msg1, R1, E1, B],
    success: A => Behavior[SOut, SOut1, Msg1, R1, E1, B]
  )(implicit ev: CanFail[E]): Behavior[SIn0, SOut1, Msg1, R1, E1, B] = {
    val _ = ev
    fromFunctionM { case (initialState, msg, r1) =>
      self.asEffect
        .foldCauseM(
          failure = { cause => failure(cause).toEffect },
          success = { a => success(a.result).toEffect.provide((a.state, msg, r1)) }
        )
        .provide((initialState, msg, r1))
    }
  }

  /**
   * Recovers from errors by accepting one behavior to execute for the case of an
   * error, and one behavior to execute for the case of success.
   */
  def foldM[SIn0 <: SIn, SOut1, Msg1 <: Msg, R1 <: R, E1, B](
    failure: E => Behavior[SIn0, SOut1, Msg1, R1, E1, B],
    success: A => Behavior[SOut, SOut1, Msg1, R1, E1, B]
  )(implicit ev: CanFail[E]): Behavior[SIn0, SOut1, Msg1, R1, E1, B] =
    fromFunctionM { case (initialState, msg, r1) =>
      self.asEffect
        .foldM(
          failure = { cause => failure(cause).toEffect },
          success = { a => success(a.result).toEffect.provide((a.state, msg, r1)) }
        )
        .provide((initialState, msg, r1))
    }

  /**
   * Exposes  the output state into the value channel.
   */
  def getState: Behavior[SIn, SOut, Msg, R, E, (SOut, A)] =
    flatMap(a => get.map(s => (s, a)))

  /**
   * Returns a behavior whose success is mapped by the specified function f.
   */
  final def map[B](f: A => B): Behavior[SIn, SOut, Msg, R, E, B] = fromEffect(asEffect.map(_.map(f)))

  /**
   * Returns a behavior with its error channel mapped using the specified
   * function. This can be used to lift a "smaller" error into a "larger"
   * error.
   */
  final def mapError[E2](f: E => E2)(implicit ev: CanFail[E]): Behavior[SIn, SOut, Msg, R, E2, A] =
    Behavior(self.asEffect.mapError(f))

  /**
   * Returns a behavior with its full cause of failure mapped using the
   * specified function. This can be used to transform errors while
   * preserving the original structure of `Cause`.
   */
  final def mapErrorCause[E2](f: Cause[E] => Cause[E2])(implicit ev: CanFail[E]): Behavior[SIn, SOut, Msg, R, E2, A] = {
    val _ = ev
    Behavior(self.asEffect.mapErrorCause(f))
  }

  final def mapResults[SOut2, B](
    f: BehaviorSuccess[SOut, A] => BehaviorSuccess[SOut2, B]
  ): Behavior[SIn, SOut2, Msg, R, E, B] =
    fromEffect(asEffect.map(res => res.transform(f)))

  final def mapState[SOut2](f: SOut => SOut2): Behavior[SIn, SOut2, Msg, R, E, A] = fromEffect(
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

  def withAnnotation[R1 <: R with Properties]: Behavior[SIn, SOut, Msg, R1, Annotated[E], Annotated[A]] =
    accessBehavior[R1] { r =>
      Behavior.fromEffect(
        r.get.withAnnotation(self.asEffect).map { case (success, map) =>
          success.map(a => (a, map))
        }
      )
    }

  //    Behavior(
//      for {
//
//      } yield ???
//      ZIO.accessM[(SIn, Msg, R1)] { case (_,_,r1) =>
//        Properties
//          .withAnnotation(asEffect)
//          .bimap(
//            identity,
//            { case (value, map) =>
//              value.map(a => (a, map))
//            }
//          )
//      }
//    )

  def zip[SIn1 <: SIn, In1 <: Msg, R1 <: R, E1 >: E, SOut1, B](
    that: Behavior[SIn1, SOut1, In1, R1, E1, B]
  ): Behavior[SIn1, (SOut, SOut1), In1, R1, E1, (A, B)] =
    Behavior((self.asEffect zip that.asEffect) map { case (left, right) => left zip right })

  def zipPar[SIn1 <: SIn, In1 <: Msg, R1 <: R, E1 >: E, SOut1, B](
    that: Behavior[SIn1, SOut1, In1, R1, E1, B]
  ): Behavior[SIn1, (SOut, SOut1), In1, R1, E1, (A, B)] =
    Behavior((self.asEffect zipPar that.asEffect) map { case (left, right) => left zip right })

  def zipWith[SIn1 <: SIn, In1 <: Msg, R1 <: R, E1 >: E, SOut1, B, S, C](
    that: Behavior[SIn1, SOut1, In1, R1, E1, B]
  )(
    f: (BehaviorSuccess[SOut, A], BehaviorSuccess[SOut1, B]) => BehaviorSuccess[S, C]
  ): Behavior[SIn1, S, In1, R1, E1, C] =
    Behavior((self.asEffect zipWith that.asEffect)(f))

  def zipWithPar[SIn1 <: SIn, In1 <: Msg, R1 <: R, E1 >: E, SOut1, B, S, C](
    that: Behavior[SIn1, SOut1, In1, R1, E1, B]
  )(
    f: (BehaviorSuccess[SOut, A], BehaviorSuccess[SOut1, B]) => BehaviorSuccess[S, C]
  ): Behavior[SIn1, S, In1, R1, E1, C] =
    Behavior((self.asEffect zipWithPar that.asEffect)(f))
}
object Behavior extends BehaviorEffectSyntax {

  def accessBehavior[R]: AccessBehaviorPartiallyApplied[R] = new AccessBehaviorPartiallyApplied[R]

  def apply[InitialState, StateOut, In, R, E, A](
    effect: BehaviorEffect[InitialState, StateOut, In, R, E, A]
  ): Behavior[InitialState, StateOut, In, R, E, A] =
    fromEffect[InitialState, StateOut, In, R, E, A](effect)

  implicit def behaviorFromFunctionM[SIn, SOut, In, R, Err, A](
    f: (SIn, In) => ZIO[R, Err, BehaviorSuccess[SOut, A]]
  ): Behavior[SIn, SOut, In, R, Err, A] =
    fromEffect[SIn, SOut, In, R, Err, A](ZIO.accessM[(SIn, In, R)] { case (state, msg, r) =>
      f(state, msg).provide(r)
    })

  def environment[R]: StatelessBehavior[Any, R, Nothing, R] = Stateless[Any, R, Nothing, R](
    ZIO.access[(Any, R)] { case (_, r) => r }
  )

  /**
   * Constructs a behavior that always fails with the given error.
   */
  def fail[E](error: E): IndieBehavior[Nothing, E, Nothing] = Fail(error)

//  def fromEffect[SIn, SOut, In, R, E, A](
//    effect: ZIO[(SIn, In, R), E, BehaviorSuccess[SOut, A]]
//  )(evState: NeedsInputState[SIn], evMsg: NeedsMsg[In]): Behavior[SIn, SOut, In, R, E, A] = {
//    val _ = (evState, evMsg) //NOTE: Suppresses the warning about these not being used
//    new Behavior[SIn, SOut, In, R, E, A] {
//      protected def behavior(state: SIn, message: In): ZIO[R, E, BehaviorSuccess[SOut, A]] =
//        effect.provideSome[R](r => (state, message, r))
//    }
//  }

  def fromEffect[SIn, SOut, Msg, R, E, A](
    effect: ZIO[(SIn, Msg, R), E, BehaviorSuccess[SOut, A]]
  ): Behavior[SIn, SOut, Msg, R, E, A] =
    FromEffect(effect)

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
   * Lifts an effectful function whose effect requires no environment into
   * a behavior that requires the input to the function.
   */
  def fromFunctionM[SIn, SOut, In, R, Err, A](
    f: (SIn, In, R) => IO[Err, BehaviorSuccess[SOut, A]]
  ): Behavior[SIn, SOut, In, R, Err, A] =
    fromEffect[SIn, SOut, In, R, Err, A](ZIO.accessM[(SIn, In, R)] { case (state, msg, r) =>
      f(state, msg, r)
    })

  /**
   * Constructs a Behavior that gets the initial state unchanged.
   */
  def get[S]: Behavior[S, S, Any, Any, Nothing, S] =
    modify(s => (s, s))

  def getMessage[In]: Behavior[Any, Any, In, Any, Nothing, In] =
    MessageHandler(ZIO.access[In](identity))

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
  def stateless[In, R, E, A](f: In => ZIO[R, E, A]): StatelessBehavior[In, R, E, A] =
    new AbstractStatelessBehavior[In, R, E, A] {
      protected def behavior(state: Any, message: In): ZIO[R, E, BehaviorSuccess[Any, A]] = f(message).map(state -> _)
    }

  /**
   * Constructs a behavior that always succeeds with the given value.
   */
  def succeed[A](value: => A): Behavior[Any, Any, Any, Any, Nothing, A] = Succeed(value)

  val unit: Behavior[Any, Any, Any, Any, Nothing, Unit] =
    succeed(())

  /**
   * Constructs a behavior from a function that produces a new state and a result given an initial state and a message.
   * This function is expected to be pure without side effects and should not throw any exceptions.
   */
  def update[S1, S2, In, A](f: (S1, In) => (S2, A)): Behavior[S1, S2, In, Any, Nothing, A] =
    Stateful(ZIO.access[(S1, In, Any)] { case (s1, msg, _) =>
      val (s2, a) = f(s1, msg)
      BehaviorSuccess(s2, a)
    })

  final case class Fail[E](error: E) extends IndieBehavior[Nothing, E, Nothing] {
    protected def behavior(state: Any, message: Any): ZIO[Any, E, BehaviorSuccess[Nothing, Nothing]] = ZIO.fail(error)
  }

  final case class FromEffect[SIn, SOut, In, Env, E, A](
    zio: ZIO[(SIn, In, Env), E, BehaviorSuccess[SOut, A]]
  ) extends Behavior[SIn, SOut, In, Env, E, A] {
    override lazy val toEffect: ZIO[(SIn, In, Env), E, BehaviorSuccess[SOut, A]] = zio
    protected def behavior(state: SIn, message: In): ZIO[Env, E, BehaviorSuccess[SOut, A]] =
      zio.provideSome[Env](env => (state, message, env))
  }

  final case class MessageHandler[-In, +Err, +A](private val zio: ZIO[In, Err, A])
      extends AbstractStatelessBehavior[In, Any, Err, A] {

    protected def behavior(state: Any, message: In): ZIO[Any, Err, BehaviorSuccess[Any, A]] =
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
  final case class Stateful[S, StateOut, In, R, E, A](
    private val effect: BehaviorEffect[S, StateOut, In, R, E, A]
  ) extends Behavior[S, StateOut, In, R, E, A] {
    protected def behavior(state: S, message: In): ZIO[R, E, BehaviorSuccess[StateOut, A]] =
      effect.provideSome[R](r => (state, message, r))
  }

  /**
   * Represents a stateless behavior that is constructed from an effect.
   */
  final case class Stateless[In, R, E, A](private val effect: ZIO[(In, R), E, A])
      extends AbstractStatelessBehavior[In, R, E, A] {
    protected def behavior(state: Any, message: In): ZIO[R, E, BehaviorSuccess[Any, A]] =
      effect.map(value => BehaviorSuccess(state, value)).provideSome[R](r => (message, r))
  }

  final class AccessBehaviorPartiallyApplied[R](private val dummy: Boolean = true) extends AnyVal {
    def apply[SIn, SOut, Msg, E, A](f: R => Behavior[SIn, SOut, Msg, R, E, A]): Behavior[SIn, SOut, Msg, R, E, A] =
      Behavior(ZIO.accessM[(SIn, Msg, R)] { case (_, _, r) =>
        f(r).toEffect
      })
  }

  final class FromEffectFn[-SIn, +SOut, -Msg, -R, +E, +A] extends ((SIn, Msg) => ZIO[R, E, (SOut, A)]) {
    def apply(initialState: SIn, message: Msg): ZIO[R, E, (SOut, A)] = ???
  }
}
