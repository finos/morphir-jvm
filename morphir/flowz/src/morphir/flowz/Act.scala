package morphir.flowz

import zio._
import zio.clock.Clock

import scala.util.Try

final case class Act[-StateIn, +StateOut, -Env, -Params, +Err, +Value](
  private[flowz] val rawEffect: ZIO[ActContext[Env, StateIn, Params], Err, StepOutputs[StateOut, Value]],
  name: Option[String] = None,
  description: Option[String] = None
) { self =>

  /**
   * Connect this Step to the given Step. By connecting the output state of this Step to the input state of the other Step
   * and by connecting the output value of this Step to the input of the other.
   */
  def >>>[SOut2, Env1 <: Env, Err1 >: Err, Output2](
    that: Act[StateOut, SOut2, Env1, Value, Err1, Output2]
  ): Act[StateIn, SOut2, Env1, Params, Err1, Output2] =
    self andThen that

  def *>[StateIn1 <: StateIn, Env1 <: Env, Params1 <: Params, Err1 >: Err, StateOut2, Output2](
    that: Act[StateIn1, StateOut2, Env1, Params1, Err1, Output2]
  ): Act[StateIn1, StateOut2, Env1, Params1, Err1, Output2] =
    Act(self.effect *> that.effect)

  def <*>[StateIn1 <: StateIn, Env1 <: Env, In1 <: Params, Err1 >: Err, StateOut2, Output2](
    that: Act[StateIn1, StateOut2, Env1, In1, Err1, Output2]
  ): Act[StateIn1, (StateOut, StateOut2), Env1, In1, Err1, (Value, Output2)] = self zip that

  def |+|[StateIn1 <: StateIn, Env1 <: Env, In1 <: Params, Err1 >: Err, StateOut2, Output2](
    that: Act[StateIn1, StateOut2, Env1, In1, Err1, Output2]
  ): Act[StateIn1, (StateOut, StateOut2), Env1, In1, Err1, (Value, Output2)] =
    Act((self.effect zipPar that.effect).map { case (left, right) => left zip right })

  def <&>[StateIn1 <: StateIn, Env1 <: Env, In1 <: Params, Err1 >: Err, StateOut2, Output2](
    that: Act[StateIn1, StateOut2, Env1, In1, Err1, Output2]
  ): Act[StateIn1, (StateOut, StateOut2), Env1, In1, Err1, (Value, Output2)] =
    Act((self.effect zipPar that.effect).map { case (left, right) => left zip right })

  def <*[StateIn1 <: StateIn, Env1 <: Env, Params1 <: Params, Err1 >: Err, StateOut2, Output2](
    that: Act[StateIn1, StateOut2, Env1, Params1, Err1, Output2]
  ): Act[StateIn1, StateOut, Env1, Params1, Err1, Value] =
    Act(self.effect <* that.effect)

  /**
   * Adapts the input provided to the Step using the provided function.
   */
  def adaptParameters[Input0](func: Input0 => Params): Act[StateIn, StateOut, Env, Input0, Err, Value] =
    new Act[StateIn, StateOut, Env, Input0, Err, Value](self.effect.provideSome { ctx =>
      ctx.copy(inputs = ctx.inputs.copy(params = func(ctx.inputs.params)))
    })

  def andThen[SOut2, Env1 <: Env, Err1 >: Err, Output2](
    that: Act[StateOut, SOut2, Env1, Value, Err1, Output2]
  ): Act[StateIn, SOut2, Env1, Params, Err1, Output2] =
    Act(ZIO.environment[ActContext[Env1, StateIn, Params]].flatMap { ctx =>
      self.effect.flatMap(out => that.effect.provide(ctx.updateInputs(out)))
    })

  def andThenEffect[Err1 >: Err, StateOut2, Output2](
    thatEffect: ZIO[Value, Err1, StepOutputs[StateOut2, Output2]]
  ): Act[StateIn, StateOut2, Env, Params, Err1, Output2] =
    Act(self.effect.map(out => out.value) andThen thatEffect)

  /**
   * Maps the success value of this flow to the specified constant value.
   */
  def as[Out2](out: => Out2): Act[StateIn, StateOut, Env, Params, Err, Out2] = self.mapValue(_ => out)

  def delay(duration: zio.duration.Duration): Act[StateIn, StateOut, Env with Clock, Params, Err, Value] =
    Act(
      for {
        ctx    <- ZIO.environment[ActContext[Env with Clock, StateIn, Params]]
        result <- self.effect.provide(ctx).delay(duration).provide(ctx.environment)
      } yield result
    )

  val effect: ZIO[ActContext[Env, StateIn, Params], Err, StepOutputs[StateOut, Value]] = rawEffect

  def flatMap[S, Env1 <: Env, P <: Params, Err1 >: Err, B](
    func: Value => Act[StateOut, S, Env1, P, Err1, B]
  ): Act[StateIn, S, Env1, P, Err1, B] =
    Act(ZIO.environment[ActContext[Env1, StateIn, P]].flatMap { ctx =>
      self.effect.flatMap(out => func(out.value).effect.provide(ctx.updateState(out.state)))
    })

  def flatten[S, Env1 <: Env, P <: Params, Err1 >: Err, B](implicit
    ev: Value <:< Act[StateOut, S, Env1, P, Err1, B]
  ): Act[StateIn, S, Env1, P, Err1, B] =
    flatMap(ev)

  def flipOutputs: Act[StateIn, Value, Env, Params, Err, StateOut] =
    self.mapOutputs { case (state, value) => (value, state) }

  def fork: ForkedStep[StateIn, StateOut, Env, Params, Err, Value] =
    Act[StateIn, Unit, Env, Params, Nothing, Fiber.Runtime[Err, StepOutputs[StateOut, Value]]](
      self.effect.fork.map { rt =>
        StepOutputs(rt)
      }
    )

  def map[StateOut2, Value2](
    fn: StepOutputs[StateOut, Value] => StepOutputs[StateOut2, Value2]
  ): Act[StateIn, StateOut2, Env, Params, Err, Value2] = new Act(
    self.effect.map(fn)
  )

  def mapValue[Out2](fn: Value => Out2): Act[StateIn, StateOut, Env, Params, Err, Out2] = Act(
    self.effect.map(success => success.mapValue(fn))
  )

  def mapEffect[Out2](fn: Value => Out2)(implicit
    ev: Err <:< Throwable
  ): Act[StateIn, StateOut, Env, Params, Throwable, Out2] =
    Act(self.effect.mapEffect(success => success.mapValue(fn)))

  def mapError[Err2](onError: Err => Err2): Act[StateIn, StateOut, Env, Params, Err2, Value] =
    Act(self.effect.mapError(onError))

  def mapOutputs[StateOut2, Output2](
    func: (StateOut, Value) => (StateOut2, Output2)
  ): Act[StateIn, StateOut2, Env, Params, Err, Output2] =
    Act(self.effect.map(out => StepOutputs.fromTuple(func(out.state, out.value))))

  def mapOutputChannels[StateOut2, Output2](
    func: StepOutputs[StateOut, Value] => StepOutputs[StateOut2, Output2]
  ): Act[StateIn, StateOut2, Env, Params, Err, Output2] =
    Act(self.effect.map(func))

  def mapState[SOut2](fn: StateOut => SOut2): Act[StateIn, SOut2, Env, Params, Err, Value] = Act(
    self.effect.map(success => success.mapState(fn))
  )

  /**
   * Executes this step and returns its value, if it succeeds, but otherwise executes the specified step.
   */
  def orElse[StateIn1 <: StateIn, Env1 <: Env, Params1 <: Params, Err2, StateOut2 >: StateOut, Value2 >: Value](
    that: => Act[StateIn1, StateOut2, Env1, Params1, Err2, Value2]
  )(implicit ev: CanFail[Err]): Act[StateIn1, StateOut2, Env1, Params1, Err2, Value2] =
    Act(self.effect orElse that.effect)

  /**
   * Returns a step that will produce the value of this step, unless it
   * fails, in which case, it will produce the value of the specified step.
   */
  def orElseEither[StateIn1 <: StateIn, Env1 <: Env, Params1 <: Params, Err2, ThatState >: StateOut, ThatValue](
    that: => Act[StateIn1, ThatState, Env1, Params1, Err2, ThatValue]
  )(implicit
    ev: CanFail[Err]
  ): Act[StateIn1, Either[StateOut, ThatState], Env1, Params1, Err2, Either[Value, ThatValue]] =
    new Act((self.effect orElseEither that.effect).map {
      case Left(outputs)  => StepOutputs(state = Left(outputs.state), value = Left(outputs.value))
      case Right(outputs) => StepOutputs(state = Right(outputs.state), value = Right(outputs.value))
    })

  /**
   * Executes this step and returns its value, if it succeeds, but otherwise fails with the specified error.
   */
  def orElseFail[State >: StateOut, Err1](error: Err1)(implicit
    ev: CanFail[Err]
  ): Act[StateIn, StateOut, Env, Params, Err1, Value] =
    orElse(Act.fail(error))

  /**
   * Executes this step and returns its value, if it succeeds, but otherwise succeeds with the specified state and value.
   */
  def orElseSucceed[State >: StateOut, Value1 >: Value](state: => State, value: => Value1)(implicit
    ev: CanFail[Err]
  ): Act[StateIn, State, Env, Params, Nothing, Value1] =
    orElse(Act.succeedWith(state = state, value = value))

  def named(name: String): Act[StateIn, StateOut, Env, Params, Err, Value] = copy(name = Option(name))
  def describe(description: String): Act[StateIn, StateOut, Env, Params, Err, Value] =
    copy(description = Option(description))

  /**
   * Repeats the step the specified number of times.
   */
  def repeatN(n: Int): Act[StateIn, StateOut, Env, Params, Err, Value] =
    Act(effect.repeatN(n))

  def repeatUntil(f: StepOutputs[StateOut, Value] => Boolean): Act[StateIn, StateOut, Env, Params, Err, Value] =
    Act(effect.repeatUntil(f))

  def repeatUntil(statePredicate: StateOut => Boolean)(
    valuePredicate: Value => Boolean
  ): Act[StateIn, StateOut, Env, Params, Err, Value] =
    Act(effect.repeatUntil(outputs => statePredicate(outputs.state) && valuePredicate(outputs.value)))

  def repeatWhile(f: StepOutputs[StateOut, Value] => Boolean): Act[StateIn, StateOut, Env, Params, Err, Value] =
    Act(effect.repeatWhile(f))

  def repeatWhileState(f: StateOut => Boolean): Act[StateIn, StateOut, Env, Params, Err, Value] =
    Act(effect.repeatWhile(outputs => f(outputs.state)))

  def repeatWhileValue(f: Value => Boolean): Act[StateIn, StateOut, Env, Params, Err, Value] =
    Act(effect.repeatWhile(outputs => f(outputs.value)))

  def retryN(n: Int)(implicit ev: CanFail[Err]): Act[StateIn, StateOut, Env, Params, Err, Value] =
    Act(effect.retryN(n))

  def retryWhile(f: StepOutputs[StateOut, Value] => Boolean): Act[StateIn, StateOut, Env, Params, Err, Value] =
    Act(effect.repeatWhile(f))

  def run(implicit
    evAnyInput: Any <:< Params,
    evAnyState: Any <:< StateIn
  ): ZIO[Env, Err, StepOutputs[StateOut, Value]] =
    self.effect.provideSome[Env](env => ActContext(environment = env, params = (), state = ()))

  def run(input: Params)(implicit evAnyState: Unit <:< StateIn): ZIO[Env, Err, StepOutputs[StateOut, Value]] =
    self.effect.provideSome[Env](env => ActContext(environment = env, params = input, state = ()))

  def run(input: Params, initialState: StateIn): ZIO[Env, Err, StepOutputs[StateOut, Value]] =
    self.effect.provideSome[Env](env => ActContext(environment = env, params = input, state = initialState))

  def run(context: ActContext[Env, StateIn, Params]): IO[Err, StepOutputs[StateOut, Value]] =
    self.effect.provide(context)

  def shiftStateToOutput: Act[StateIn, Unit, Env, Params, Err, (StateOut, Value)] =
    Act(effect.map(success => StepOutputs(state = (), value = (success.state, success.value))))

  /**
   * Maps the output state value of this step to the specified constant value.
   */
  def stateAs[StateOut2](stateOut: => StateOut2): Act[StateIn, StateOut2, Env, Params, Err, Value] =
    self.mapState(_ => stateOut)

  /**
   * Takes the output state and makes it also available as the result value of this flow.
   */
  def stateAsValue: Act[StateIn, StateOut, Env, Params, Err, StateOut] =
    self.mapOutputs((state, _) => (state, state))

  def tap[Env1 <: Env, Err1 >: Err](
    func: (StateOut, Value) => ZIO[Env1, Err1, Any]
  ): Act[StateIn, StateOut, Env1, Params, Err1, Value] =
    Act(
      ZIO
        .environment[ActContext[Env1, StateIn, Params]]
        .flatMap(ctx => self.effect.tap(out => func(out.state, out.value).provide(ctx.environment)))
    )

  def tapState[Env1 <: Env, Err1 >: Err](
    func: StateOut => ZIO[Env1, Err1, Any]
  ): Act[StateIn, StateOut, Env1, Params, Err1, Value] =
    Act(
      ZIO
        .environment[ActContext[Env1, StateIn, Params]]
        .flatMap(ctx => self.effect.tap(out => func(out.state).provide(ctx.environment)))
    )

  def tapValue[Env1 <: Env, Err1 >: Err](
    func: Value => ZIO[Env1, Err1, Any]
  ): Act[StateIn, StateOut, Env1, Params, Err1, Value] =
    Act(
      ZIO
        .environment[ActContext[Env1, StateIn, Params]]
        .flatMap(ctx => self.effect.tap(out => func(out.value).provide(ctx.environment)))
    )

  def transformEff[StateOut2, Output2](
    func: (StateOut, Value) => (StateOut2, Output2)
  )(implicit ev: Err <:< Throwable): Act[StateIn, StateOut2, Env, Params, Throwable, Output2] =
    Act(self.effect.mapEffect(out => StepOutputs.fromTuple(func(out.state, out.value))))

  /**
   * Make the state and the output value the same by making the state equal to the output.
   */
  def valueAsState: Act[StateIn, Value, Env, Params, Err, Value] =
    self.mapOutputs { case (_, value) =>
      (value, value)
    }

  def zip[StateIn1 <: StateIn, Env1 <: Env, In1 <: Params, Err1 >: Err, StateOut2, Output2](
    that: Act[StateIn1, StateOut2, Env1, In1, Err1, Output2]
  ): Act[StateIn1, (StateOut, StateOut2), Env1, In1, Err1, (Value, Output2)] =
    Act((self.effect zip that.effect).map { case (left, right) => left zip right })

  def zipPar[StateIn1 <: StateIn, Env1 <: Env, In1 <: Params, Err1 >: Err, StateOut2, Output2](
    that: Act[StateIn1, StateOut2, Env1, In1, Err1, Output2]
  ): Act[StateIn1, (StateOut, StateOut2), Env1, In1, Err1, (Value, Output2)] =
    Act((self.effect zipPar that.effect).map { case (left, right) => left zip right })

  def zipWith[
    StateIn1 <: StateIn,
    Env1 <: Env,
    In1 <: Params,
    Err1 >: Err,
    StateOut2,
    Output2,
    FinalState,
    FinalOutput
  ](that: Act[StateIn1, StateOut2, Env1, In1, Err1, Output2])(
    f: (
      StepOutputs[StateOut, Value],
      StepOutputs[StateOut2, Output2]
    ) => StepOutputs[FinalState, FinalOutput]
  ): Act[StateIn1, FinalState, Env1, In1, Err1, FinalOutput] =
    Act((self.effect zipWith that.effect)(f))

  def zipWithPar[
    StateIn1 <: StateIn,
    Env1 <: Env,
    In1 <: Params,
    Err1 >: Err,
    StateOut2,
    Output2,
    FinalState,
    FinalOutput
  ](that: Act[StateIn1, StateOut2, Env1, In1, Err1, Output2])(
    f: (StepOutputs[StateOut, Value], StepOutputs[StateOut2, Output2]) => StepOutputs[FinalState, FinalOutput]
  ): Act[StateIn1, FinalState, Env1, In1, Err1, FinalOutput] =
    Act((self.effect zipWithPar that.effect)(f))
}

object Act extends ActCompanion[Any] {

  def apply[StateIn, StateOut, Env, Params, Err, Value](
    func: (StateIn, Params) => ZIO[Env, Err, StepOutputs[StateOut, Value]]
  ): Act[StateIn, StateOut, Env, Params, Err, Value] =
    Act(
      ZIO
        .environment[ActContext[Env, StateIn, Params]]
        .flatMap(ctx => func(ctx.inputs.state, ctx.inputs.params).provide(ctx.environment))
    )

  def apply[StateIn, StateOut, Env, Params, Err, Value](name: String)(
    func: (StateIn, Params) => ZIO[Env, Err, StepOutputs[StateOut, Value]]
  ): Act[StateIn, StateOut, Env, Params, Err, Value] =
    Act(
      ZIO
        .environment[ActContext[Env, StateIn, Params]]
        .flatMap(ctx => func(ctx.inputs.state, ctx.inputs.params).provide(ctx.environment)),
      name = Option(name)
    )

  def apply[StateIn, StateOut, Env, Params, Err, Value](name: String, description: String)(
    func: (StateIn, Params) => ZIO[Env, Err, StepOutputs[StateOut, Value]]
  ): Act[StateIn, StateOut, Env, Params, Err, Value] =
    Act(
      ZIO
        .environment[ActContext[Env, StateIn, Params]]
        .flatMap(ctx => func(ctx.inputs.state, ctx.inputs.params).provide(ctx.environment)),
      name = Option(name),
      description = Option(description)
    )

  def fromEither[Err, Value](value: Either[Err, Value]): Act[Any, Unit, Any, Any, Err, Value] =
    Act(for {
      _     <- ZIO.environment[ActContext.having.AnyInputs]
      value <- ZIO.fromEither(value)
    } yield StepOutputs.fromValue(value))

  def fromFunction[In, Out](func: In => Out): Act[Any, Out, Any, In, Nothing, Out] =
    Act(ZIO.environment[ActContext.having.Parameters[In]].map { ctx =>
      val value = func(ctx.inputs.params)
      StepOutputs(value = value, state = value)
    })

  def fromOption[Value](value: => Option[Value]): Act[Any, Unit, Any, Any, Option[Nothing], Value] =
    Act(for {
      _     <- ZIO.environment[ActContext.having.AnyInputs]
      value <- ZIO.fromOption(value)
    } yield StepOutputs.fromValue(value))

  def fromOutputs[State, Output](
    channels: StepOutputs[State, Output]
  ): Act[Any, State, Any, Any, Nothing, Output] =
    Act(ZIO.succeed(channels))

  def fromTry[Value](value: => Try[Value]): Act[Any, Unit, Any, Any, Throwable, Value] =
    Act(for {
      _     <- ZIO.environment[ActContext.having.AnyInputs]
      value <- ZIO.fromTry(value)
    } yield StepOutputs.fromValue(value))

  def inputs[StateIn, Params]: Act[StateIn, (StateIn, Params), Any, Params, Nothing, (StateIn, Params)] =
    Act(
      ZIO
        .environment[ActContext[Any, StateIn, Params]]
        .map(ctx =>
          StepOutputs(state = (ctx.inputs.state, ctx.inputs.params), value = (ctx.inputs.state, ctx.inputs.params))
        )
    )

  def join[State, Err, Output](
    fiber: Fiber[Err, StepOutputs[State, Output]]
  ): Act[Any, State, Any, Any, Err, Output] =
    Act(fiber.join)

  def stage[StateIn, StateOut, Env, Params, Err, Out](
    func: (StateIn, Params) => Act[StateIn, StateOut, Env, Params, Err, Out]
  ): Act[StateIn, StateOut, Env, Params, Err, Out] =
    Act.context[Env, StateIn, Params].flatMap(ctx => func(ctx.inputs.state, ctx.inputs.params))

  def state[State]: Act[State, State, Any, Any, Nothing, State] = Act(
    ZIO.environment[ActContext[Any, State, Any]].map(ctx => StepOutputs.setBoth(ctx.inputs.state))
  )

  def stateful[StateIn, Params, StateOut, Out](
    func: (StateIn, Params) => (StateOut, Out)
  ): Act[StateIn, StateOut, Any, Params, Nothing, Out] =
    Act(ZIO.environment[ActContext.having.AnyEnv[StateIn, Params]].map { ctx =>
      val (state, value) = func(ctx.inputs.state, ctx.inputs.params)
      StepOutputs(state = state, value = value)
    })

  def statefulEffect[StateIn, Params, StateOut, Out](
    func: (StateIn, Params) => (StateOut, Out)
  ): Act[StateIn, StateOut, Any, Params, Throwable, Out] =
    Act(ZIO.environment[ActContext.having.AnyEnv[StateIn, Params]].mapEffect { ctx =>
      val (state, value) = func(ctx.inputs.state, ctx.inputs.params)
      StepOutputs(state = state, value = value)
    })

  /**
   * Create a `Step` by providing a function that takes in some state and parameters and returns a tuple of
   * the output state and the result.
   */
  def step[StateIn, StateOut, Params, Out](
    func: (StateIn, Params) => (StateOut, Out)
  ): Act[StateIn, StateOut, Any, Params, Throwable, Out] =
    Act(ZIO.environment[ActContext[Any, StateIn, Params]].mapEffect { ctx =>
      val (state, value) = func(ctx.inputs.state, ctx.inputs.params)
      StepOutputs(state = state, value = value)
    })

  /**
   * Returns a step with the empty value.
   */
  val none: Act[Any, Option[Nothing], Any, Any, Nothing, Option[Nothing]] =
    Act(ZIO.environment[ActContext.having.AnyInputs].as(StepOutputs.none))

  /**
   * A step that succeeds with a unit value.
   */
  val unit: Act[Any, Unit, Any, Any, Nothing, Unit] =
    Act(ZIO.environment[ActContext.having.AnyInputs].as(StepOutputs.unit))

  def withStateAs[State](state: => State): Act[Any, State, Any, Any, Nothing, Unit] =
    Act(ZIO.succeed(StepOutputs.fromState(state)))

  def withValue[Value](value: => Value): Act[Any, Unit, Any, Any, Nothing, Value] =
    Act(ZIO.succeed(StepOutputs.fromValue(value)))

  def withStateAndValue[A](valueAndSate: => A): Act[Any, A, Any, Any, Nothing, A] =
    Act(ZIO.succeed(valueAndSate).map(StepOutputs.setBoth(_)))

  def withEnvironment[Env, Err, State, Value](
    func: Env => ZIO[Env, Err, (State, Value)]
  ): Act[Any, State, Env, Any, Err, Value] =
    Act(
      ZIO
        .environment[ActContext[Env, Any, Any]]
        .flatMap(ctx =>
          func(ctx.environment).map { case (state, value) =>
            StepOutputs(state = state, value = value)
          }.provide(ctx.environment)
        )
    )

  def withEnvironment[Env, Err, State, Value](
    effect: ZIO[Env, Err, (State, Value)]
  ): Act[Any, State, Env, Any, Err, Value] =
    Act(
      ZIO
        .environment[ActContext[Env, Any, Any]]
        .flatMap(ctx =>
          effect.map { case (state, value) =>
            StepOutputs(state = state, value = value)
          }.provide(ctx.environment)
        )
    )

  def withOutputs[A](valueAndSate: A): Act[Any, A, Any, Any, Nothing, A] =
    succeedWith(state = valueAndSate, value = valueAndSate)

  def withParams[Env, Params, Err, Out](func: Params => ZIO[Env, Err, Out]): Act[Any, Any, Env, Params, Err, Out] =
    Act.parameters[Params].flatMap { params =>
      Act.fromEffect(func(params))
    }
}
