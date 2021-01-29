package morphir.flowz

import zio._
import zio.clock.Clock

import scala.util.Try

final case class Stage[-StateIn, +StateOut, -Env, -Params, +Err, +Value](
  private[flowz] val rawEffect: ZIO[StageContext[Env, StateIn, Params], Err, StepOutputs[StateOut, Value]],
  name: Option[String] = None,
  description: Option[String] = None
) { self =>

  /**
   * Connect this Step to the given Step. By connecting the output state of this Step to the input state of the other Step
   * and by connecting the output value of this Step to the input of the other.
   */
  def >>>[SOut2, Env1 <: Env, Err1 >: Err, Output2](
    that: Stage[StateOut, SOut2, Env1, Value, Err1, Output2]
  ): Stage[StateIn, SOut2, Env1, Params, Err1, Output2] =
    self andThen that

  def *>[StateIn1 <: StateIn, Env1 <: Env, Params1 <: Params, Err1 >: Err, StateOut2, Output2](
    that: Stage[StateIn1, StateOut2, Env1, Params1, Err1, Output2]
  ): Stage[StateIn1, StateOut2, Env1, Params1, Err1, Output2] =
    Stage(self.effect *> that.effect)

  def <*>[StateIn1 <: StateIn, Env1 <: Env, In1 <: Params, Err1 >: Err, StateOut2, Output2](
    that: Stage[StateIn1, StateOut2, Env1, In1, Err1, Output2]
  ): Stage[StateIn1, (StateOut, StateOut2), Env1, In1, Err1, (Value, Output2)] = self zip that

  def |+|[StateIn1 <: StateIn, Env1 <: Env, In1 <: Params, Err1 >: Err, StateOut2, Output2](
    that: Stage[StateIn1, StateOut2, Env1, In1, Err1, Output2]
  ): Stage[StateIn1, (StateOut, StateOut2), Env1, In1, Err1, (Value, Output2)] =
    Stage((self.effect zipPar that.effect).map { case (left, right) => left zip right })

  def <&>[StateIn1 <: StateIn, Env1 <: Env, In1 <: Params, Err1 >: Err, StateOut2, Output2](
    that: Stage[StateIn1, StateOut2, Env1, In1, Err1, Output2]
  ): Stage[StateIn1, (StateOut, StateOut2), Env1, In1, Err1, (Value, Output2)] =
    Stage((self.effect zipPar that.effect).map { case (left, right) => left zip right })

  def <*[StateIn1 <: StateIn, Env1 <: Env, Params1 <: Params, Err1 >: Err, StateOut2, Output2](
    that: Stage[StateIn1, StateOut2, Env1, Params1, Err1, Output2]
  ): Stage[StateIn1, StateOut, Env1, Params1, Err1, Value] =
    Stage(self.effect <* that.effect)

  /**
   * Adapts the input provided to the Step using the provided function.
   */
  def adaptParameters[Input0](func: Input0 => Params): Stage[StateIn, StateOut, Env, Input0, Err, Value] =
    new Stage[StateIn, StateOut, Env, Input0, Err, Value](self.effect.provideSome { ctx =>
      ctx.copy(inputs = ctx.inputs.copy(params = func(ctx.inputs.params)))
    })

  def andThen[SOut2, Env1 <: Env, Err1 >: Err, Output2](
    that: Stage[StateOut, SOut2, Env1, Value, Err1, Output2]
  ): Stage[StateIn, SOut2, Env1, Params, Err1, Output2] =
    Stage(ZIO.environment[StageContext[Env1, StateIn, Params]].flatMap { ctx =>
      self.effect.flatMap(out => that.effect.provide(ctx.updateInputs(out)))
    })

  def andThenEffect[Err1 >: Err, StateOut2, Output2](
    thatEffect: ZIO[Value, Err1, StepOutputs[StateOut2, Output2]]
  ): Stage[StateIn, StateOut2, Env, Params, Err1, Output2] =
    Stage(self.effect.map(out => out.value) andThen thatEffect)

  /**
   * Maps the success value of this flow to the specified constant value.
   */
  def as[Out2](out: => Out2): Stage[StateIn, StateOut, Env, Params, Err, Out2] = self.mapValue(_ => out)

  def delay(duration: zio.duration.Duration): Stage[StateIn, StateOut, Env with Clock, Params, Err, Value] =
    Stage(
      for {
        ctx    <- ZIO.environment[StageContext[Env with Clock, StateIn, Params]]
        result <- self.effect.provide(ctx).delay(duration).provide(ctx.environment)
      } yield result
    )

  val effect: ZIO[StageContext[Env, StateIn, Params], Err, StepOutputs[StateOut, Value]] = rawEffect

  def flatMap[S, Env1 <: Env, P <: Params, Err1 >: Err, B](
    func: Value => Stage[StateOut, S, Env1, P, Err1, B]
  ): Stage[StateIn, S, Env1, P, Err1, B] =
    Stage(ZIO.environment[StageContext[Env1, StateIn, P]].flatMap { ctx =>
      self.effect.flatMap(out => func(out.value).effect.provide(ctx.updateState(out.state)))
    })

  def flatten[S, Env1 <: Env, P <: Params, Err1 >: Err, B](implicit
    ev: Value <:< Stage[StateOut, S, Env1, P, Err1, B]
  ): Stage[StateIn, S, Env1, P, Err1, B] =
    flatMap(ev)

  def flipOutputs: Stage[StateIn, Value, Env, Params, Err, StateOut] =
    self.mapOutputs { case (state, value) => (value, state) }

  def fork: ForkedStep[StateIn, StateOut, Env, Params, Err, Value] =
    Stage[StateIn, Unit, Env, Params, Nothing, Fiber.Runtime[Err, StepOutputs[StateOut, Value]]](
      self.effect.fork.map { rt =>
        StepOutputs(rt)
      }
    )

  def map[StateOut2, Value2](
    fn: StepOutputs[StateOut, Value] => StepOutputs[StateOut2, Value2]
  ): Stage[StateIn, StateOut2, Env, Params, Err, Value2] = new Stage(
    self.effect.map(fn)
  )

  def mapValue[Out2](fn: Value => Out2): Stage[StateIn, StateOut, Env, Params, Err, Out2] = Stage(
    self.effect.map(success => success.mapValue(fn))
  )

  def mapEffect[Out2](fn: Value => Out2)(implicit
    ev: Err <:< Throwable
  ): Stage[StateIn, StateOut, Env, Params, Throwable, Out2] =
    Stage(self.effect.mapEffect(success => success.mapValue(fn)))

  def mapError[Err2](onError: Err => Err2): Stage[StateIn, StateOut, Env, Params, Err2, Value] =
    Stage(self.effect.mapError(onError))

  def mapOutputs[StateOut2, Output2](
    func: (StateOut, Value) => (StateOut2, Output2)
  ): Stage[StateIn, StateOut2, Env, Params, Err, Output2] =
    Stage(self.effect.map(out => StepOutputs.fromTuple(func(out.state, out.value))))

  def mapOutputChannels[StateOut2, Output2](
    func: StepOutputs[StateOut, Value] => StepOutputs[StateOut2, Output2]
  ): Stage[StateIn, StateOut2, Env, Params, Err, Output2] =
    Stage(self.effect.map(func))

  def mapState[SOut2](fn: StateOut => SOut2): Stage[StateIn, SOut2, Env, Params, Err, Value] = Stage(
    self.effect.map(success => success.mapState(fn))
  )

  /**
   * Executes this step and returns its value, if it succeeds, but otherwise executes the specified step.
   */
  def orElse[StateIn1 <: StateIn, Env1 <: Env, Params1 <: Params, Err2, StateOut2 >: StateOut, Value2 >: Value](
    that: => Stage[StateIn1, StateOut2, Env1, Params1, Err2, Value2]
  )(implicit ev: CanFail[Err]): Stage[StateIn1, StateOut2, Env1, Params1, Err2, Value2] =
    Stage(self.effect orElse that.effect)

  /**
   * Returns a step that will produce the value of this step, unless it
   * fails, in which case, it will produce the value of the specified step.
   */
  def orElseEither[StateIn1 <: StateIn, Env1 <: Env, Params1 <: Params, Err2, ThatState >: StateOut, ThatValue](
    that: => Stage[StateIn1, ThatState, Env1, Params1, Err2, ThatValue]
  )(implicit
    ev: CanFail[Err]
  ): Stage[StateIn1, Either[StateOut, ThatState], Env1, Params1, Err2, Either[Value, ThatValue]] =
    new Stage((self.effect orElseEither that.effect).map {
      case Left(outputs)  => StepOutputs(state = Left(outputs.state), value = Left(outputs.value))
      case Right(outputs) => StepOutputs(state = Right(outputs.state), value = Right(outputs.value))
    })

  /**
   * Executes this step and returns its value, if it succeeds, but otherwise fails with the specified error.
   */
  def orElseFail[State >: StateOut, Err1](error: Err1)(implicit
    ev: CanFail[Err]
  ): Stage[StateIn, StateOut, Env, Params, Err1, Value] =
    orElse(Stage.fail(error))

  /**
   * Executes this step and returns its value, if it succeeds, but otherwise succeeds with the specified state and value.
   */
  def orElseSucceed[State >: StateOut, Value1 >: Value](state: => State, value: => Value1)(implicit
    ev: CanFail[Err]
  ): Stage[StateIn, State, Env, Params, Nothing, Value1] =
    orElse(Stage.succeedWith(state = state, value = value))

  def named(name: String): Stage[StateIn, StateOut, Env, Params, Err, Value] = copy(name = Option(name))
  def describe(description: String): Stage[StateIn, StateOut, Env, Params, Err, Value] =
    copy(description = Option(description))

  /**
   * Repeats the step the specified number of times.
   */
  def repeatN(n: Int): Stage[StateIn, StateOut, Env, Params, Err, Value] =
    Stage(effect.repeatN(n))

  def repeatUntil(f: StepOutputs[StateOut, Value] => Boolean): Stage[StateIn, StateOut, Env, Params, Err, Value] =
    Stage(effect.repeatUntil(f))

  def repeatUntil(statePredicate: StateOut => Boolean)(
    valuePredicate: Value => Boolean
  ): Stage[StateIn, StateOut, Env, Params, Err, Value] =
    Stage(effect.repeatUntil(outputs => statePredicate(outputs.state) && valuePredicate(outputs.value)))

  def repeatWhile(f: StepOutputs[StateOut, Value] => Boolean): Stage[StateIn, StateOut, Env, Params, Err, Value] =
    Stage(effect.repeatWhile(f))

  def repeatWhileState(f: StateOut => Boolean): Stage[StateIn, StateOut, Env, Params, Err, Value] =
    Stage(effect.repeatWhile(outputs => f(outputs.state)))

  def repeatWhileValue(f: Value => Boolean): Stage[StateIn, StateOut, Env, Params, Err, Value] =
    Stage(effect.repeatWhile(outputs => f(outputs.value)))

  def retryN(n: Int)(implicit ev: CanFail[Err]): Stage[StateIn, StateOut, Env, Params, Err, Value] =
    Stage(effect.retryN(n))

  def retryWhile(f: StepOutputs[StateOut, Value] => Boolean): Stage[StateIn, StateOut, Env, Params, Err, Value] =
    Stage(effect.repeatWhile(f))

  def run(implicit
    evAnyInput: Any <:< Params,
    evAnyState: Any <:< StateIn
  ): ZIO[Env, Err, StepOutputs[StateOut, Value]] =
    self.effect.provideSome[Env](env => StageContext(environment = env, params = (), state = ()))

  def run(input: Params)(implicit evAnyState: Unit <:< StateIn): ZIO[Env, Err, StepOutputs[StateOut, Value]] =
    self.effect.provideSome[Env](env => StageContext(environment = env, params = input, state = ()))

  def run(input: Params, initialState: StateIn): ZIO[Env, Err, StepOutputs[StateOut, Value]] =
    self.effect.provideSome[Env](env => StageContext(environment = env, params = input, state = initialState))

  def run(context: StageContext[Env, StateIn, Params]): IO[Err, StepOutputs[StateOut, Value]] =
    self.effect.provide(context)

  def shiftStateToOutput: Stage[StateIn, Unit, Env, Params, Err, (StateOut, Value)] =
    Stage(effect.map(success => StepOutputs(state = (), value = (success.state, success.value))))

  /**
   * Maps the output state value of this step to the specified constant value.
   */
  def stateAs[StateOut2](stateOut: => StateOut2): Stage[StateIn, StateOut2, Env, Params, Err, Value] =
    self.mapState(_ => stateOut)

  /**
   * Takes the output state and makes it also available as the result value of this flow.
   */
  def stateAsValue: Stage[StateIn, StateOut, Env, Params, Err, StateOut] =
    self.mapOutputs((state, _) => (state, state))

  def tap[Env1 <: Env, Err1 >: Err](
    func: (StateOut, Value) => ZIO[Env1, Err1, Any]
  ): Stage[StateIn, StateOut, Env1, Params, Err1, Value] =
    Stage(
      ZIO
        .environment[StageContext[Env1, StateIn, Params]]
        .flatMap(ctx => self.effect.tap(out => func(out.state, out.value).provide(ctx.environment)))
    )

  def tapState[Env1 <: Env, Err1 >: Err](
    func: StateOut => ZIO[Env1, Err1, Any]
  ): Stage[StateIn, StateOut, Env1, Params, Err1, Value] =
    Stage(
      ZIO
        .environment[StageContext[Env1, StateIn, Params]]
        .flatMap(ctx => self.effect.tap(out => func(out.state).provide(ctx.environment)))
    )

  def tapValue[Env1 <: Env, Err1 >: Err](
    func: Value => ZIO[Env1, Err1, Any]
  ): Stage[StateIn, StateOut, Env1, Params, Err1, Value] =
    Stage(
      ZIO
        .environment[StageContext[Env1, StateIn, Params]]
        .flatMap(ctx => self.effect.tap(out => func(out.value).provide(ctx.environment)))
    )

  def transformEff[StateOut2, Output2](
    func: (StateOut, Value) => (StateOut2, Output2)
  )(implicit ev: Err <:< Throwable): Stage[StateIn, StateOut2, Env, Params, Throwable, Output2] =
    Stage(self.effect.mapEffect(out => StepOutputs.fromTuple(func(out.state, out.value))))

  /**
   * Make the state and the output value the same by making the state equal to the output.
   */
  def valueAsState: Stage[StateIn, Value, Env, Params, Err, Value] =
    self.mapOutputs { case (_, value) =>
      (value, value)
    }

  def zip[StateIn1 <: StateIn, Env1 <: Env, In1 <: Params, Err1 >: Err, StateOut2, Output2](
    that: Stage[StateIn1, StateOut2, Env1, In1, Err1, Output2]
  ): Stage[StateIn1, (StateOut, StateOut2), Env1, In1, Err1, (Value, Output2)] =
    Stage((self.effect zip that.effect).map { case (left, right) => left zip right })

  def zipPar[StateIn1 <: StateIn, Env1 <: Env, In1 <: Params, Err1 >: Err, StateOut2, Output2](
    that: Stage[StateIn1, StateOut2, Env1, In1, Err1, Output2]
  ): Stage[StateIn1, (StateOut, StateOut2), Env1, In1, Err1, (Value, Output2)] =
    Stage((self.effect zipPar that.effect).map { case (left, right) => left zip right })

  def zipWith[
    StateIn1 <: StateIn,
    Env1 <: Env,
    In1 <: Params,
    Err1 >: Err,
    StateOut2,
    Output2,
    FinalState,
    FinalOutput
  ](that: Stage[StateIn1, StateOut2, Env1, In1, Err1, Output2])(
    f: (
      StepOutputs[StateOut, Value],
      StepOutputs[StateOut2, Output2]
    ) => StepOutputs[FinalState, FinalOutput]
  ): Stage[StateIn1, FinalState, Env1, In1, Err1, FinalOutput] =
    Stage((self.effect zipWith that.effect)(f))

  def zipWithPar[
    StateIn1 <: StateIn,
    Env1 <: Env,
    In1 <: Params,
    Err1 >: Err,
    StateOut2,
    Output2,
    FinalState,
    FinalOutput
  ](that: Stage[StateIn1, StateOut2, Env1, In1, Err1, Output2])(
    f: (StepOutputs[StateOut, Value], StepOutputs[StateOut2, Output2]) => StepOutputs[FinalState, FinalOutput]
  ): Stage[StateIn1, FinalState, Env1, In1, Err1, FinalOutput] =
    Stage((self.effect zipWithPar that.effect)(f))
}

object Stage extends StepCompanion[Any] {

  def apply[StateIn, StateOut, Env, Params, Err, Value](
    func: (StateIn, Params) => ZIO[Env, Err, StepOutputs[StateOut, Value]]
  ): Stage[StateIn, StateOut, Env, Params, Err, Value] =
    Stage(
      ZIO
        .environment[StageContext[Env, StateIn, Params]]
        .flatMap(ctx => func(ctx.inputs.state, ctx.inputs.params).provide(ctx.environment))
    )

  def apply[StateIn, StateOut, Env, Params, Err, Value](name: String)(
    func: (StateIn, Params) => ZIO[Env, Err, StepOutputs[StateOut, Value]]
  ): Stage[StateIn, StateOut, Env, Params, Err, Value] =
    Stage(
      ZIO
        .environment[StageContext[Env, StateIn, Params]]
        .flatMap(ctx => func(ctx.inputs.state, ctx.inputs.params).provide(ctx.environment)),
      name = Option(name)
    )

  def apply[StateIn, StateOut, Env, Params, Err, Value](name: String, description: String)(
    func: (StateIn, Params) => ZIO[Env, Err, StepOutputs[StateOut, Value]]
  ): Stage[StateIn, StateOut, Env, Params, Err, Value] =
    Stage(
      ZIO
        .environment[StageContext[Env, StateIn, Params]]
        .flatMap(ctx => func(ctx.inputs.state, ctx.inputs.params).provide(ctx.environment)),
      name = Option(name),
      description = Option(description)
    )

  def fromEither[Err, Value](value: Either[Err, Value]): Stage[Any, Unit, Any, Any, Err, Value] =
    Stage(for {
      _     <- ZIO.environment[StageContext.having.AnyInputs]
      value <- ZIO.fromEither(value)
    } yield StepOutputs.fromValue(value))

  def fromFunction[In, Out](func: In => Out): Stage[Any, Out, Any, In, Nothing, Out] =
    Stage(ZIO.environment[StageContext.having.Parameters[In]].map { ctx =>
      val value = func(ctx.inputs.params)
      StepOutputs(value = value, state = value)
    })

  def fromOption[Value](value: => Option[Value]): Stage[Any, Unit, Any, Any, Option[Nothing], Value] =
    Stage(for {
      _     <- ZIO.environment[StageContext.having.AnyInputs]
      value <- ZIO.fromOption(value)
    } yield StepOutputs.fromValue(value))

  def fromOutputs[State, Output](channels: StepOutputs[State, Output]): Stage[Any, State, Any, Any, Nothing, Output] =
    Stage(ZIO.succeed(channels))

  def fromTry[Value](value: => Try[Value]): Stage[Any, Unit, Any, Any, Throwable, Value] =
    Stage(for {
      _     <- ZIO.environment[StageContext.having.AnyInputs]
      value <- ZIO.fromTry(value)
    } yield StepOutputs.fromValue(value))

  def inputs[StateIn, Params]: Stage[StateIn, (StateIn, Params), Any, Params, Nothing, (StateIn, Params)] =
    Stage(
      ZIO
        .environment[StageContext[Any, StateIn, Params]]
        .map(ctx =>
          StepOutputs(state = (ctx.inputs.state, ctx.inputs.params), value = (ctx.inputs.state, ctx.inputs.params))
        )
    )

  def join[State, Err, Output](
    fiber: Fiber[Err, StepOutputs[State, Output]]
  ): Stage[Any, State, Any, Any, Err, Output] =
    Stage(fiber.join)

  def stage[StateIn, StateOut, Env, Params, Err, Out](
    func: (StateIn, Params) => Stage[StateIn, StateOut, Env, Params, Err, Out]
  ): Stage[StateIn, StateOut, Env, Params, Err, Out] =
    Stage.context[Env, StateIn, Params].flatMap(ctx => func(ctx.inputs.state, ctx.inputs.params))

  def state[State]: Stage[State, State, Any, Any, Nothing, State] = Stage(
    ZIO.environment[StageContext[Any, State, Any]].map(ctx => StepOutputs.setBoth(ctx.inputs.state))
  )

  def stateful[StateIn, Params, StateOut, Out](
    func: (StateIn, Params) => (StateOut, Out)
  ): Stage[StateIn, StateOut, Any, Params, Nothing, Out] =
    Stage(ZIO.environment[StageContext.having.AnyEnv[StateIn, Params]].map { ctx =>
      val (state, value) = func(ctx.inputs.state, ctx.inputs.params)
      StepOutputs(state = state, value = value)
    })

  def statefulEffect[StateIn, Params, StateOut, Out](
    func: (StateIn, Params) => (StateOut, Out)
  ): Stage[StateIn, StateOut, Any, Params, Throwable, Out] =
    Stage(ZIO.environment[StageContext.having.AnyEnv[StateIn, Params]].mapEffect { ctx =>
      val (state, value) = func(ctx.inputs.state, ctx.inputs.params)
      StepOutputs(state = state, value = value)
    })

  /**
   * Create a `Step` by providing a function that takes in some state and parameters and returns a tuple of
   * the output state and the result.
   */
  def step[StateIn, StateOut, Params, Out](
    func: (StateIn, Params) => (StateOut, Out)
  ): Stage[StateIn, StateOut, Any, Params, Throwable, Out] =
    Stage(ZIO.environment[StageContext[Any, StateIn, Params]].mapEffect { ctx =>
      val (state, value) = func(ctx.inputs.state, ctx.inputs.params)
      StepOutputs(state = state, value = value)
    })

  /**
   * Returns a step with the empty value.
   */
  val none: Stage[Any, Option[Nothing], Any, Any, Nothing, Option[Nothing]] =
    Stage(ZIO.environment[StageContext.having.AnyInputs].as(StepOutputs.none))

  /**
   * A step that succeeds with a unit value.
   */
  val unit: Stage[Any, Unit, Any, Any, Nothing, Unit] =
    Stage(ZIO.environment[StageContext.having.AnyInputs].as(StepOutputs.unit))

  def withStateAs[State](state: => State): Stage[Any, State, Any, Any, Nothing, Unit] =
    Stage(ZIO.succeed(StepOutputs.fromState(state)))

  def withValue[Value](value: => Value): Stage[Any, Unit, Any, Any, Nothing, Value] =
    Stage(ZIO.succeed(StepOutputs.fromValue(value)))

  def withStateAndValue[A](valueAndSate: => A): Stage[Any, A, Any, Any, Nothing, A] =
    Stage(ZIO.succeed(valueAndSate).map(StepOutputs.setBoth(_)))

  def withEnvironment[Env, Err, State, Value](
    func: Env => ZIO[Env, Err, (State, Value)]
  ): Stage[Any, State, Env, Any, Err, Value] =
    Stage(
      ZIO
        .environment[StageContext[Env, Any, Any]]
        .flatMap(ctx =>
          func(ctx.environment).map { case (state, value) =>
            StepOutputs(state = state, value = value)
          }.provide(ctx.environment)
        )
    )

  def withEnvironment[Env, Err, State, Value](
    effect: ZIO[Env, Err, (State, Value)]
  ): Stage[Any, State, Env, Any, Err, Value] =
    Stage(
      ZIO
        .environment[StageContext[Env, Any, Any]]
        .flatMap(ctx =>
          effect.map { case (state, value) =>
            StepOutputs(state = state, value = value)
          }.provide(ctx.environment)
        )
    )

  def withOutputs[A](valueAndSate: A): Stage[Any, A, Any, Any, Nothing, A] =
    succeedWith(state = valueAndSate, value = valueAndSate)

  def withParams[Env, Params, Err, Out](func: Params => ZIO[Env, Err, Out]): Stage[Any, Any, Env, Params, Err, Out] =
    Stage.parameters[Params].flatMap { params =>
      Stage.fromEffect(func(params))
    }
}
