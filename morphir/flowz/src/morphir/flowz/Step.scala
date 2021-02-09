package morphir.flowz

import morphir.flowz.instrumentation.InstrumentationEvent
import zio._

/**
 * A step is a purely functional description of a computation that requires
 * an environment `R`, an initial state of `SIn` and an input/update message `Msg`.
 * It may fail with either an `E` or succeed with an updated state `SOut` and a result `A`.
 */
abstract class Step[-SIn, +SOut, -Msg, -R, +E, +A] { self =>
  import Step._

  /**
   * Returns a step that executes both this step and the specified step,
   * in parallel, returning the result of the provided step. If either side fails,
   * then the other side will be interrupted, interrupting the result.
   */
  def &>[SIn1 <: SIn, In1 <: Msg, R1 <: R, E1 >: E, SOut1, B, S, C](
    that: Step[SIn1, SOut1, In1, R1, E1, B]
  ): Step[SIn1, SOut1, In1, R1, E1, B] =
    self.zipWithPar(that)((_, b) => b)

  /**
   * Connect this Step to the given Step by connecting the output state of this Step to the input state
   * of the other Step and by connecting the result of this Step to the input of the other.
   */
  def >>>[SOut2, R1 <: R, E1 >: E, B](
    that: Step[SOut, SOut2, A, R1, E1, B]
  ): Step[SIn, SOut2, Msg, R1, E1, B] =
    self andThen that

  /**
   * A variant of flatMap that ignores the values (result and state)  produced by this step.
   */
  def *>[SIn1 <: SIn, Msg1 <: Msg, R1 <: R, E1 >: E, SOut1, B](
    that: Step[SIn1, SOut1, Msg1, R1, E1, B]
  ): Step[SIn1, SOut1, Msg1, R1, E1, B] =
    Step(self.asEffect *> that.asEffect)

  /**
   * An operator alias for zip.
   */
  def <*>[SIn1 <: SIn, Msg1 <: Msg, R1 <: R, E1 >: E, SOut1, B](
    that: Step[SIn1, SOut1, Msg1, R1, E1, B]
  ): Step[SIn1, (SOut, SOut1), Msg1, R1, E1, (A, B)] = self zip that

  /**
   * Returns a step that executes both this step and the specified step,
   * in parallel, this step's result is returned. If either side fails,
   * then the other side will be interrupted, thus interrupting the result.
   */
  def <&[SIn1 <: SIn, In1 <: Msg, R1 <: R, E1 >: E, SOut1, B, S, C](
    that: Step[SIn1, SOut1, In1, R1, E1, B]
  ): Step[SIn1, SOut, In1, R1, E1, A] =
    self.zipWithPar(that)((a, _) => a)

  /**
   * An operator alias for zip.
   */
  def <&>[SIn1 <: SIn, Msg1 <: Msg, R1 <: R, E1 >: E, SOut1, B](
    that: Step[SIn1, SOut1, Msg1, R1, E1, B]
  ): Step[SIn1, (SOut, SOut1), Msg1, R1, E1, (A, B)] = self zipPar that

  /**
   * An operator alias for zipPar.
   */
  def |+|[SIn1 <: SIn, Msg1 <: Msg, R1 <: R, E1 >: E, SOut1, B](
    that: Step[SIn1, SOut1, Msg1, R1, E1, B]
  ): Step[SIn1, (SOut, SOut1), Msg1, R1, E1, (A, B)] = self zipPar that

  /**
   * Sequences the specified step after this step, but ignores the
   * values (result and state) produced by the step.
   */
  def <*[SIn1 <: SIn, In1 <: Msg, R1 <: R, E1 >: E, SOut1, B](
    that: Step[SIn1, SOut1, In1, R1, E1, B]
  ): Step[SIn1, SOut, In1, R1, E1, A] = Step(self.asEffect <* that.asEffect)

  /**
   * Executes the given step upon the successful execution of this step.
   */
  def andThen[SOut2, R1 <: R, E1 >: E, B](
    that: Step[SOut, SOut2, A, R1, E1, B]
  ): Step[SIn, SOut2, Msg, R1, E1, B] =
    Step(ZIO.accessM[(SIn, Msg, R1)] { case (_, _, r) =>
      self.asEffect.flatMap(success => that.asEffect.provide((success.state, success.result, r)))
    })

  /**
   * Maps the success value of this step to a constant value.
   */
  final def as[B](b: => B): Step[SIn, SOut, Msg, R, E, B] = map(_ => b)

  /**
   * Get this Step as an effect.
   */
  protected final lazy val asEffect: ZBehavior[SIn, SOut, Msg, R, E, A] = toEffect

  /**
   * Defines the underlying behavior of this `Step`.
   */
  protected def behavior(state: SIn, message: Msg): ZIO[R, E, StepSuccess[SOut, A]]

  /**
   * Returns a step whose failure and success channels have been mapped by the specified pair of
   * functions, f and g.
   */
  def bimap[E2, B](f: E => E2, g: A => B)(implicit ev: CanFail[E]): Step[SIn, SOut, Msg, R, E2, B] =
    Step(ZIO.accessM[(SIn, Msg, R)] { env =>
      self.asEffect.provide(env).bimap(f, _.map(g))
    })

  /**
   * Returns a step whose failure and success have been lifted into an
   * `Either`. The resulting computation cannot fail, because the failure case
   * has been exposed as part of the `Either` success case.
   */
  def either[S >: SOut <: SIn](implicit ev: CanFail[E]): Step[S, S, Msg, R, Nothing, Either[E, A]] =
    foldM(
      failure = e => modify(s => (s, Left(e))),
      success = a => modify(s => (s, Right(a)))
    )

  /**
   * Returns a step that models the execution of this step, followed by
   * the passing of its value to the specified continuation function `k`,
   * followed by the step that it returns.
   *
   * {{{
   * val parsed = readFile("foo.txt").flatMap(file => parseFile(file))
   * }}}
   */
  def flatMap[SOut1, In1 <: Msg, R1 <: R, E1 >: E, B](
    k: A => Step[SOut, SOut1, In1, R1, E1, B]
  ): Step[SIn, SOut1, In1, R1, E1, B] =
    Step[SIn, SOut1, In1, R1, E1, B](
      ZIO.accessM[(SIn, In1, R1)] { case (_, msg, r) =>
        asEffect.flatMap { res: StepSuccess[SOut, A] =>
          k(res.result).asEffect.provide((res.state, msg, r))
        }
      }
    )

  /**
   * Folds over the failed or successful results of this step to yield
   * a step that does not fail, but succeeds with the value of the left
   * or right function passed to `fold`.
   */
  def fold[S >: SOut <: SIn, B](failure: E => B, success: A => B)(implicit
    ev: CanFail[E]
  ): Step[S, S, Msg, R, Nothing, B] =
    self.foldM(e => modify(s => (s, failure(e): B)), a => modify(s => (s, success(a))))

  /**
   * A more powerful version of `foldM` that allows recovering from any kind of failure except interruptions.
   */
  def foldCauseM[SIn0 <: SIn, SOut1, Msg1 <: Msg, R1 <: R, E1, B](
    failure: Cause[E] => Step[SIn0, SOut1, Msg1, R1, E1, B],
    success: A => Step[SOut, SOut1, Msg1, R1, E1, B]
  )(implicit ev: CanFail[E]): Step[SIn0, SOut1, Msg1, R1, E1, B] = {
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
   * Recovers from errors by accepting one step to execute for the case of an
   * error, and one step to execute for the case of success.
   */
  def foldM[SIn0 <: SIn, SOut1, Msg1 <: Msg, R1 <: R, E1, B](
    failure: E => Step[SIn0, SOut1, Msg1, R1, E1, B],
    success: A => Step[SOut, SOut1, Msg1, R1, E1, B]
  )(implicit ev: CanFail[E]): Step[SIn0, SOut1, Msg1, R1, E1, B] =
    fromFunctionM { case (initialState, msg, r1) =>
      self.asEffect
        .foldM(
          failure = { cause => failure(cause).toEffect },
          success = { a => success(a.result).toEffect.provide((a.state, msg, r1)) }
        )
        .provide((initialState, msg, r1))
    }

  /**
   * Exposes the output state into the value channel.
   */
  def getState: Step[SIn, SOut, Msg, R, E, (SOut, A)] =
    flatMap(a => get.map(s => (s, a)))

  def label: Option[String] = None

  /**
   * Returns a step whose success is mapped by the specified function f.
   */
  final def map[B](f: A => B): Step[SIn, SOut, Msg, R, E, B] = fromEffect(asEffect.map(_.map(f)))

  /**
   * Returns a step with its error channel mapped using the specified
   * function. This can be used to lift a "smaller" error into a "larger"
   * error.
   */
  final def mapError[E2](f: E => E2)(implicit ev: CanFail[E]): Step[SIn, SOut, Msg, R, E2, A] =
    Step(self.asEffect.mapError(f))

  /**
   * Returns a step with its full cause of failure mapped using the
   * specified function. This can be used to transform errors while
   * preserving the original structure of `Cause`.
   */
  final def mapErrorCause[E2](f: Cause[E] => Cause[E2])(implicit ev: CanFail[E]): Step[SIn, SOut, Msg, R, E2, A] = {
    val _ = ev
    Step(self.asEffect.mapErrorCause(f))
  }

  final def mapResults[SOut2, B](
    f: StepSuccess[SOut, A] => StepSuccess[SOut2, B]
  ): Step[SIn, SOut2, Msg, R, E, B] =
    fromEffect(asEffect.map(res => res.transform(f)))

  final def mapState[SOut2](f: SOut => SOut2): Step[SIn, SOut2, Msg, R, E, A] = fromEffect(
    asEffect.map(_.mapState(f))
  )

  /**
   * Provide the step all its required inputs and its environment
   */
  final def provide(initialState: SIn, message: Msg, environment: R): IndieStep[SOut, E, A] =
    Step(asEffect.provide((initialState, message, environment)))

  /**
   * Provide the step all its required inputs and its environment
   */
  final def provide(message: Msg)(implicit ev1: Any <:< SIn, ev2: Any <:< R): IndieStep[SOut, E, A] =
    Step(asEffect.provide(((), message, ())))

  /**
   * Provide the step all its required inputs and its environment
   */
  final def provide(initialState: SIn, message: Msg)(implicit ev1: Any <:< R): IndieStep[SOut, E, A] =
    Step(asEffect.provide((initialState, message, ())))

  /**
   * Provide the step with its environment.
   */
  final def provideEnvironment(environment: R): Step[SIn, SOut, Msg, Any, E, A] =
    Step(ZIO.accessM[(SIn, Msg, Any)] { case (initialState, message, _) =>
      asEffect.provide((initialState, message, environment))
    })

  /**
   * Provide the step with its initial state and message.
   */
  final def provideInputs(initialState: SIn, message: Msg): Step[Any, SOut, Any, R, E, A] =
    Step(ZIO.accessM[(Any, Any, R)] { case (_, _, r) => asEffect.provide((initialState, message, r)) })

  /**
   * Provide the step with its update message.
   */
  final def provideMessage(message: Msg): Step[SIn, SOut, Any, R, E, A] =
    Step(ZIO.accessM[(SIn, Any, R)] { case (initialState, _, r) => asEffect.provide((initialState, message, r)) })

  /**
   * Provide the step with its initial state
   */
  final def provideState(initialState: SIn): Step[Any, SOut, Msg, R, E, A] =
    Step(asEffect.provideState(initialState))

  /**
   * Runs the step.
   */
  final def run(implicit ev1: Any <:< SIn, ev2: Any <:< Msg): ZIO[R with StepRuntimeEnv, E, StepSuccess[SOut, A]] =
    run((), ())

  /**
   * Runs the step.
   */
  final def run(state: SIn, message: Msg): ZIO[R with StepRuntimeEnv, E, StepSuccess[SOut, A]] =
    for {
      uid           <- StepUid.nextUid
      labelResolved <- ZIO.succeed(label getOrElse "N/A")
      _ <- iLog.trace(
             InstrumentationEvent.runningStep(s"Running Step[Label=$labelResolved; Uid=$uid;]", uid, labelResolved)
           )
      result <- behavior(state, message)
    } yield result

  /**
   * Runs the step.
   */
  final def run(message: Msg)(implicit ev: Any <:< SIn): ZIO[R with StepRuntimeEnv, E, StepSuccess[SOut, A]] =
    run((), message)

  /**
   * Runs the step.
   */
  final def runResult(implicit ev1: Any <:< SIn, ev2: Any <:< Msg): ZIO[R with StepRuntimeEnv, E, A] =
    run((), ()).map(_.result)

  /**
   * Returns a step that effectfully "peeks" at the success of this behavior.
   *
   * {{{
   * readFile("data.json").tap(putStrLn)
   * }}}
   */
  def tap[R1 <: R, E1 >: E](
    func: StepSuccess[SOut, A] => ZIO[R1, E1, Any]
  ): Step[SIn, SOut, Msg, R1, E1, A] =
    Step[SIn, SOut, Msg, R1, E1, A](
      ZIO.accessM[(SIn, Msg, R1)] { case (_, _, r1) =>
        self.asEffect.tap(success => func(success).provide(r1))
      }
    )

  def toEffect: ZIO[(SIn, Msg, R), E, StepSuccess[SOut, A]] = ZIO.accessM[(SIn, Msg, R)] { case (stateIn, msg, r) =>
    behavior(stateIn, msg).provide(r)
  }

  def withAnnotation[R1 <: R with Properties]: Step[SIn, SOut, Msg, R1, Annotated[E], Annotated[A]] =
    accessStep[R1] { r =>
      Step.fromEffect(
        r.get.withAnnotation(self.asEffect).map { case (success, map) =>
          success.map(a => (a, map))
        }
      )
    }

  def withLabel(label: String): Step[SIn, SOut, Msg, R, E, A] =
    StepWithMetadata(label = Option(label), underlyingStep = self)

  def zip[SIn1 <: SIn, In1 <: Msg, R1 <: R, E1 >: E, SOut1, B](
    that: Step[SIn1, SOut1, In1, R1, E1, B]
  ): Step[SIn1, (SOut, SOut1), In1, R1, E1, (A, B)] =
    Step((self.asEffect zip that.asEffect) map { case (left, right) => left zip right })

  def zipPar[SIn1 <: SIn, In1 <: Msg, R1 <: R, E1 >: E, SOut1, B](
    that: Step[SIn1, SOut1, In1, R1, E1, B]
  ): Step[SIn1, (SOut, SOut1), In1, R1, E1, (A, B)] =
    Step((self.asEffect zipPar that.asEffect) map { case (left, right) => left zip right })

  def zipWith[SIn1 <: SIn, In1 <: Msg, R1 <: R, E1 >: E, SOut1, B, S, C](
    that: Step[SIn1, SOut1, In1, R1, E1, B]
  )(
    f: (StepSuccess[SOut, A], StepSuccess[SOut1, B]) => StepSuccess[S, C]
  ): Step[SIn1, S, In1, R1, E1, C] =
    Step((self.asEffect zipWith that.asEffect)(f))

  def zipWithPar[SIn1 <: SIn, In1 <: Msg, R1 <: R, E1 >: E, SOut1, B, S, C](
    that: Step[SIn1, SOut1, In1, R1, E1, B]
  )(
    f: (StepSuccess[SOut, A], StepSuccess[SOut1, B]) => StepSuccess[S, C]
  ): Step[SIn1, S, In1, R1, E1, C] =
    Step((self.asEffect zipWithPar that.asEffect)(f))
}
object Step extends StepArities with ZBehaviorSyntax {

  def accessStep[R]: AccessStepPartiallyApplied[R]  = new AccessStepPartiallyApplied[R]
  def accessService[R]: AccessServicePartially[R]   = new AccessServicePartially[R]
  def accessServiceM[R]: AccessServiceMPartially[R] = new AccessServiceMPartially[R]

  def apply[InitialState, StateOut, In, R, E, A](
    effect: ZBehavior[InitialState, StateOut, In, R, E, A]
  ): Step[InitialState, StateOut, In, R, E, A] =
    fromEffect[InitialState, StateOut, In, R, E, A](effect)

  implicit def behaviorFromFunctionM[SIn, SOut, In, R, Err, A](
    f: (SIn, In) => ZIO[R, Err, StepSuccess[SOut, A]]
  ): Step[SIn, SOut, In, R, Err, A] =
    fromEffect[SIn, SOut, In, R, Err, A](ZIO.accessM[(SIn, In, R)] { case (state, msg, r) =>
      f(state, msg).provide(r)
    })

  def environment[R]: StatelessStep[Any, R, Nothing, R] = Stateless[Any, R, Nothing, R](
    ZIO.access[(Any, R)] { case (_, r) => r }
  )

  /**
   * Constructs a behavior that always fails with the given error.
   */
  def fail[E](error: E): IndieStep[Nothing, E, Nothing] = Fail(error)

//  def fromEffect[SIn, SOut, In, R, E, A](
//    effect: ZIO[(SIn, In, R), E, StepSuccess[SOut, A]]
//  )(evState: NeedsInputState[SIn], evMsg: NeedsMsg[In]): Step[SIn, SOut, In, R, E, A] = {
//    val _ = (evState, evMsg) //NOTE: Suppresses the warning about these not being used
//    new Step[SIn, SOut, In, R, E, A] {
//      protected def behavior(state: SIn, message: In): ZIO[R, E, StepSuccess[SOut, A]] =
//        effect.provideSome[R](r => (state, message, r))
//    }
//  }

  def fromEffect[SIn, SOut, Msg, R, E, A](
    effect: ZIO[(SIn, Msg, R), E, StepSuccess[SOut, A]]
  ): Step[SIn, SOut, Msg, R, E, A] =
    FromEffect(effect)

  /**
   * Create a behavior by providing a possibly impure function.
   *
   * For example:
   *
   * {{{
   *   val intConverter = Step.fromFunction { numberStr:String =>
   *      numberStr.toInt
   *   }
   * }}}
   */
  def fromFunction[In, Out](f: In => Out): FuncStep[In, Out] =
    MessageHandler(ZIO.accessM[In](input => ZIO.effect(f(input))))

  /**
   * Lifts an effectful function whose effect requires no environment into
   * a behavior that requires the input to the function.
   */
  def fromFunctionM[SIn, SOut, In, R, Err, A](
    f: (SIn, In, R) => IO[Err, StepSuccess[SOut, A]]
  ): Step[SIn, SOut, In, R, Err, A] =
    fromEffect[SIn, SOut, In, R, Err, A](ZIO.accessM[(SIn, In, R)] { case (state, msg, r) =>
      f(state, msg, r)
    })

  /**
   * Creates a behavior from a typical `ZIO` effect which does not have input state and message components.
   */
  def fromZIO[R, E, A](effect: ZIO[R, E, A]): ZIOStep[R, E, A] = FromZIO[R, E, A](effect)

  /**
   * Constructs a Step that gets the initial state unchanged.
   */
  def get[S]: Step[S, S, Any, Any, Nothing, S] =
    modify(s => (s, s))

  def getMessage[In]: Step[Any, Any, In, Any, Nothing, In] =
    MessageHandler(ZIO.access[In](identity))

  /**
   * Constructs a Step from a modify function.
   */
  def modify[S1, S2, A](f: S1 => (S2, A)): Step[S1, S2, Any, Any, Nothing, A] =
    Modify(f)

  def outputting[S, Value](state: S, value: Value): IndieStep[S, Nothing, Value] =
    SetOutputs(newState = state, value = value)

  /**
   * Accesses the specified service in the environment of the behavior.
   */
  def service[R: Tag]: ZIOStep[Has[R], Nothing, R] =
    fromZIO(ZIO.service[R])

  /**
   * Constructs a Step that sets the state to the specified value.å
   */
  def set[S](state: S): Step[Any, S, Any, Any, Nothing, Unit] = modify(_ => (state, ()))

  /**
   * Constructs a stateless behavior from a function that produces an effect.
   */
  def stateless[In, R, E, A](f: In => ZIO[R, E, A]): StatelessStep[In, R, E, A] =
    new AbstractStatelessStep[In, R, E, A] {
      protected def behavior(state: Any, message: In): ZIO[R, E, StepSuccess[Any, A]] = f(message).map(state -> _)
    }

  /**
   * Constructs a behavior that always succeeds with the given value.
   */
  def succeed[A](value: => A): Step[Any, Any, Any, Any, Nothing, A] = Succeed(value)

  val unit: Step[Any, Any, Any, Any, Nothing, Unit] =
    succeed(())

  /**
   * Constructs a behavior from a function that produces a new state and a result given an initial state and a message.
   * This function is expected to be pure without side effects and should not throw any exceptions.
   */
  def update[S1, S2, In, A](f: (S1, In) => (S2, A)): Step[S1, S2, In, Any, Nothing, A] =
    Stateful(ZIO.access[(S1, In, Any)] { case (s1, msg, _) =>
      val (s2, a) = f(s1, msg)
      StepSuccess(s2, a)
    })

  final case class Fail[E](error: E) extends IndieStep[Nothing, E, Nothing] {
    protected def behavior(state: Any, message: Any): ZIO[Any, E, StepSuccess[Nothing, Nothing]] = ZIO.fail(error)
  }

  final case class FromEffect[SIn, SOut, In, Env, E, A](
    zio: ZIO[(SIn, In, Env), E, StepSuccess[SOut, A]]
  ) extends Step[SIn, SOut, In, Env, E, A] {
    override lazy val toEffect: ZIO[(SIn, In, Env), E, StepSuccess[SOut, A]] = zio
    protected def behavior(state: SIn, message: In): ZIO[Env, E, StepSuccess[SOut, A]] =
      zio.provideSome[Env](env => (state, message, env))
  }

  final case class FromZIO[-R, +E, +A](zio: ZIO[R, E, A]) extends Step[Any, Any, Any, R, E, A] {
    protected def behavior(state: Any, message: Any): ZIO[R, E, StepSuccess[Any, A]] =
      zio.map(a => StepSuccess(state = state, result = a))
  }

  final case class MessageHandler[-In, +Err, +A](private val zio: ZIO[In, Err, A])
      extends AbstractStatelessStep[In, Any, Err, A] {

    protected def behavior(state: Any, message: In): ZIO[Any, Err, StepSuccess[Any, A]] =
      zio.map(result => StepSuccess(state = state, result = result)).provide(message)

  }

  final case class Modify[-S1, +S2, +A](func: S1 => (S2, A)) extends Step[S1, S2, Any, Any, Nothing, A] {

    protected def behavior(state: S1, message: Any): ZIO[Any, Nothing, StepSuccess[S2, A]] =
      ZIO.effectTotal(func(state))

  }

  final case class SetOutputs[S, A](newState: S, value: A) extends IndieStep[S, Nothing, A] {
    protected def behavior(state: Any, message: Any): ZIO[Any, Nothing, StepSuccess[S, A]] =
      ZIO.succeed((newState, value))
  }

  final case class Succeed[+A](value: A) extends Step[Any, Any, Any, Any, Nothing, A] {
    protected def behavior(state: Any, message: Any): ZIO[Any, Nothing, StepSuccess[Any, A]] =
      ZIO.succeed(StepSuccess(state, value))
  }

  /**
   * Represents a stateful behavior that is constructed from an effect.
   */
  final case class Stateful[S, StateOut, In, R, E, A](
    private val effect: ZBehavior[S, StateOut, In, R, E, A]
  ) extends Step[S, StateOut, In, R, E, A] {
    protected def behavior(state: S, message: In): ZIO[R, E, StepSuccess[StateOut, A]] =
      effect.provideSome[R](r => (state, message, r))
  }

  /**
   * Represents a stateless behavior that is constructed from an effect.
   */
  final case class Stateless[In, R, E, A](private val effect: ZIO[(In, R), E, A])
      extends AbstractStatelessStep[In, R, E, A] {
    protected def behavior(state: Any, message: In): ZIO[R, E, StepSuccess[Any, A]] =
      effect.map(value => StepSuccess(state, value)).provideSome[R](r => (message, r))
  }

  final class AccessServicePartially[R](private val dummy: Boolean = true) extends AnyVal {
    def apply[A](f: R => A)(implicit tag: Tag[R]): ZIOStep[Has[R], Nothing, A] =
      Step.fromZIO(ZIO.service[R].flatMap(r => ZIO.effectTotal(f(r))))
  }

  final class AccessServiceMPartially[R](private val dummy: Boolean = true) extends AnyVal {
    def apply[E, A](f: R => IO[E, A])(implicit tag: Tag[R]): ZIOStep[Has[R], E, A] =
      Step.fromZIO(ZIO.service[R].flatMap(r => f(r)))
  }

  final class AccessStepPartiallyApplied[R](private val dummy: Boolean = true) extends AnyVal {
    def apply[SIn, SOut, Msg, E, A](f: R => Step[SIn, SOut, Msg, R, E, A]): Step[SIn, SOut, Msg, R, E, A] =
      Step(ZIO.accessM[(SIn, Msg, R)] { case (_, _, r) =>
        f(r).toEffect
      })
  }

  final class FromEffectFn[-SIn, +SOut, -Msg, -R, +E, +A] extends ((SIn, Msg) => ZIO[R, E, (SOut, A)]) {
    def apply(initialState: SIn, message: Msg): ZIO[R, E, (SOut, A)] = ???
  }

  final case class StepWithMetadata[-SIn, +SOut, -Msg, -R, +E, +A](
    override val label: Option[String],
    private val underlyingStep: Step[SIn, SOut, Msg, R, E, A]
  ) extends Step[SIn, SOut, Msg, R, E, A] {

    /**
     * Defines the underlying behavior of this `Step`.
     */
    protected def behavior(state: SIn, message: Msg): ZIO[R, E, StepSuccess[SOut, A]] =
      underlyingStep.behavior(state, message)
  }
}

object stepExample extends App {
  import zio.console.Console
  import zio.logging.LogLevel
  import morphir.flowz.instrumentation.InstrumentationLogging

  def defineStep[SIn, SOut, Msg, R, E, A](label: String)(
    theStep: Step[SIn, SOut, Msg, R, E, A]
  ): Step[SIn, SOut, Msg, R with StepRuntimeEnv, E, A] =
    theStep.withLabel(label)
  //TODO: This is where you could do something like add an Aspect to the step

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    val step1 = defineStep("Say Hi")(Step.accessServiceM[Console.Service](_.putStrLn("Hi")))

    step1.run.exitCode.provideCustomLayer(
      StepUidGenerator.live ++ InstrumentationLogging.console(logLevel = LogLevel.Trace)
    )
  }
}
