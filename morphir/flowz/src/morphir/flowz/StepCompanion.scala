package morphir.flowz

import zio._

abstract class StepCompanion[-BaseEnv] {

  /**
   * A step that uses its parameters to build another Step.
   */
  def accessParametersM[SIn, SOut, R, P, E, A](func: P => Stage[SIn, SOut, R, P, E, A]): Stage[SIn, SOut, R, P, E, A] =
    Stage(ZIO.accessM[StageContext[R, SIn, P]](ctx => func(ctx.inputs.params).effect))

  /** Defines a step that does not rely on state. */
  def act[Params, Out](f: Params => Out): Activity[Any, Params, Throwable, Out] =
    Stage(
      ZIO
        .environment[StageContext.having.Parameters[Params]]
        .mapEffect(ctx => StepOutputs.assignBoth(f(ctx.inputs.params)))
    )

  /** Defines a step that does not rely on state. */
  def act[Params, Out](name: String)(f: Params => Out): Activity[Any, Params, Throwable, Out] =
    Stage(
      ZIO
        .environment[StageContext.having.Parameters[Params]]
        .mapEffect(ctx => StepOutputs.assignBoth(f(ctx.inputs.params))),
      name = Option(name)
    )

  /** Defines a step that does not rely on state. */
  def act[Params, Out](name: String, description: String)(f: Params => Out): Activity[Any, Params, Throwable, Out] =
    Stage(
      ZIO
        .environment[StageContext.having.Parameters[Params]]
        .mapEffect(ctx => StepOutputs.assignBoth(f(ctx.inputs.params))),
      name = Option(name),
      description = Option(description)
    )

  /** Defines a step that performs some effect which does not rely on state. */
  def activity[Env, Params, Err, Value](func: Params => ZIO[Env, Err, Value]): Activity[Env, Params, Err, Value] =
    Stage(ZIO.accessM[StageContext[Env, Any, Params]] { ctx =>
      func(ctx.inputs.params).map(value => StepOutputs(state = value, value = value)).provide(ctx.environment)
    })

  /** Defines a step that performs some effect which does not rely on state. */
  def activity[Env, Params, Err, Value](name: String)(
    func: Params => ZIO[Env, Err, Value]
  ): Activity[Env, Params, Err, Value] =
    Stage(
      ZIO.accessM[StageContext[Env, Any, Params]] { ctx =>
        func(ctx.inputs.params).map(value => StepOutputs(state = value, value = value)).provide(ctx.environment)
      },
      name = Option(name)
    )

  def activity[Env, Params, Err, Value](name: String, description: String)(
    func: Params => ZIO[Env, Err, Value]
  ): Activity[Env, Params, Err, Value] =
    Stage(
      ZIO.accessM[StageContext[Env, Any, Params]] { ctx =>
        func(ctx.inputs.params).map(value => StepOutputs(state = value, value = value)).provide(ctx.environment)
      },
      name = Option(name),
      description = Option(description)
    )

  def context[Env, StateIn, Params]: Stage[StateIn, StateIn, Env, Params, Nothing, StageContext[Env, StateIn, Params]] =
    Stage(
      ZIO
        .environment[StageContext[Env, StateIn, Params]]
        .map(ctx => StepOutputs(value = ctx, state = ctx.inputs.state))
    )

  def describe[StateIn, StateOut, Env, Params, Err, Value](description: String)(
    step: Stage[StateIn, StateOut, Env, Params, Err, Value]
  ): Stage[StateIn, StateOut, Env, Params, Err, Value] =
    step.describe(description)

  def effect[Value](value: => Value): TaskStep[Any, Value] =
    Stage(ZIO.environment[StageContext.having.AnyInputs].mapEffect(_ => StepOutputs.fromValue(value)))

  def environment[Env <: BaseEnv]: Stage[Any, Env, Env, Any, Nothing, Env] =
    Stage(ZIO.environment[StageContext.having.Environment[Env]].map(ctx => StepOutputs.setBoth(ctx.environment)))

  def fail[Err](error: Err): Stage[Any, Nothing, BaseEnv, Any, Err, Nothing] =
    Stage(ZIO.environment[StageContext.having.AnyInputs] *> ZIO.fail(error))

  def fromEffect[R, E, A](effect: ZIO[R, E, A]): Stage[Any, Any, R, Any, E, A] =
    Stage(
      ZIO
        .environment[StageContext[R, Any, Any]]
        .flatMap(ctx => effect.map(ctx.toOutputs(_)).provide(ctx.environment))
    )

  def fromEffect[P, R, E, A](func: P => ZIO[R, E, A]): Stage[Any, Unit, R, P, E, A] =
    Stage(
      ZIO
        .environment[StageContext[R, Any, P]]
        .flatMap(ctx => func(ctx.inputs.params).map(StepOutputs.fromValue(_)).provide(ctx.environment))
    )

  /**
   * Get the state.
   */
  def get[State]: Stage[State, State, Any, Any, Nothing, State] =
    modify[State, State, State](s => (s, s))

  def mapN[S0, R, P, Err, SA, A, SB, B, SC, C](flowA: Stage[S0, SA, R, P, Err, A], flowB: Stage[S0, SB, R, P, Err, B])(
    f: (StepOutputs[SA, A], StepOutputs[SB, B]) => StepOutputs[SC, C]
  ): Stage[S0, SC, R, P, Err, C] =
    flowA.zipWith(flowB)(f)

  def mapParN[S0, R, P, Err, SA, A, SB, B, SC, C](
    flowA: Stage[S0, SA, R, P, Err, A],
    flowB: Stage[S0, SB, R, P, Err, B]
  )(
    f: (StepOutputs[SA, A], StepOutputs[SB, B]) => StepOutputs[SC, C]
  ): Stage[S0, SC, R, P, Err, C] =
    flowA.zipWithPar(flowB)(f)

  def mapParN[S0, R, P, Err, SA, A, SB, B, SC, C, SD, D](
    flowA: Stage[S0, SA, R, P, Err, A],
    flowB: Stage[S0, SB, R, P, Err, B],
    flowC: Stage[S0, SC, R, P, Err, C]
  )(
    f: (StepOutputs[SA, A], StepOutputs[SB, B], StepOutputs[SC, C]) => StepOutputs[SD, D]
  ): Stage[S0, SD, R, P, Err, D] =
    (flowA <&> flowB <&> flowC).mapOutputChannels { case StepOutputs(((sa, sb), sc), ((a, b), c)) =>
      val outsA = StepOutputs(state = sa, value = a)
      val outsB = StepOutputs(state = sb, value = b)
      val outsC = StepOutputs(state = sc, value = c)
      f(outsA, outsB, outsC)
    }

  def mapParN[S0, R, P, Err, SA, A, SB, B, SC, C, SD, D, SF, F](
    flowA: Stage[S0, SA, R, P, Err, A],
    flowB: Stage[S0, SB, R, P, Err, B],
    flowC: Stage[S0, SC, R, P, Err, C],
    flowD: Stage[S0, SD, R, P, Err, D]
  )(
    f: (StepOutputs[SA, A], StepOutputs[SB, B], StepOutputs[SC, C], StepOutputs[SD, D]) => StepOutputs[SF, F]
  ): Stage[S0, SF, R, P, Err, F] =
    (flowA <&> flowB <&> flowC <&> flowD).mapOutputChannels {
      case StepOutputs((((sa, sb), sc), sd), (((a, b), c), d)) =>
        val outsA = StepOutputs(state = sa, value = a)
        val outsB = StepOutputs(state = sb, value = b)
        val outsC = StepOutputs(state = sc, value = c)
        val outsD = StepOutputs(state = sd, value = d)
        f(outsA, outsB, outsC, outsD)
    }

  def mapParN[S0, R, P, Err, S1, A1, S2, A2, S3, A3, S4, A4, S5, A5, S6, A6](
    flow1: Stage[S0, S1, R, P, Err, A1],
    flow2: Stage[S0, S2, R, P, Err, A2],
    flow3: Stage[S0, S3, R, P, Err, A3],
    flow4: Stage[S0, S4, R, P, Err, A4],
    flow5: Stage[S0, S5, R, P, Err, A5]
  )(
    f: (
      StepOutputs[S1, A1],
      StepOutputs[S2, A2],
      StepOutputs[S3, A3],
      StepOutputs[S4, A4],
      StepOutputs[S5, A5]
    ) => StepOutputs[S6, A6]
  ): Stage[S0, S6, R, P, Err, A6] =
    (flow1 <&> flow2 <&> flow3 <&> flow4 <&> flow5).mapOutputChannels {
      case StepOutputs(((((sa, sb), sc), sd), se), ((((a, b), c), d), e)) =>
        val outsA = StepOutputs(state = sa, value = a)
        val outsB = StepOutputs(state = sb, value = b)
        val outsC = StepOutputs(state = sc, value = c)
        val outsD = StepOutputs(state = sd, value = d)
        val outsE = StepOutputs(state = se, value = e)
        f(outsA, outsB, outsC, outsD, outsE)
    }

  def mapParN[S0, R, P, Err, S1, A1, S2, A2, S3, A3, S4, A4, S5, A5, S6, A6, S7, A7](
    flow1: Stage[S0, S1, R, P, Err, A1],
    flow2: Stage[S0, S2, R, P, Err, A2],
    flow3: Stage[S0, S3, R, P, Err, A3],
    flow4: Stage[S0, S4, R, P, Err, A4],
    flow5: Stage[S0, S5, R, P, Err, A5],
    flow6: Stage[S0, S6, R, P, Err, A6]
  )(
    func: (
      StepOutputs[S1, A1],
      StepOutputs[S2, A2],
      StepOutputs[S3, A3],
      StepOutputs[S4, A4],
      StepOutputs[S5, A5],
      StepOutputs[S6, A6]
    ) => StepOutputs[S7, A7]
  ): Stage[S0, S7, R, P, Err, A7] =
    (flow1 <&> flow2 <&> flow3 <&> flow4 <&> flow5 <&> flow6).mapOutputChannels {
      case StepOutputs((((((sa, sb), sc), sd), se), sf), (((((a, b), c), d), e), f)) =>
        val outsA = StepOutputs(state = sa, value = a)
        val outsB = StepOutputs(state = sb, value = b)
        val outsC = StepOutputs(state = sc, value = c)
        val outsD = StepOutputs(state = sd, value = d)
        val outsE = StepOutputs(state = se, value = e)
        val outsF = StepOutputs(state = sf, value = f)
        func(outsA, outsB, outsC, outsD, outsE, outsF)
    }

  def mapParN[S0, R, P, Err, S1, A1, S2, A2, S3, A3, S4, A4, S5, A5, S6, A6, S7, A7, S8, A8](
    flow1: Stage[S0, S1, R, P, Err, A1],
    flow2: Stage[S0, S2, R, P, Err, A2],
    flow3: Stage[S0, S3, R, P, Err, A3],
    flow4: Stage[S0, S4, R, P, Err, A4],
    flow5: Stage[S0, S5, R, P, Err, A5],
    flow6: Stage[S0, S6, R, P, Err, A6],
    flow7: Stage[S0, S7, R, P, Err, A7]
  )(
    func: (
      StepOutputs[S1, A1],
      StepOutputs[S2, A2],
      StepOutputs[S3, A3],
      StepOutputs[S4, A4],
      StepOutputs[S5, A5],
      StepOutputs[S6, A6],
      StepOutputs[S7, A7]
    ) => StepOutputs[S8, A8]
  ): Stage[S0, S8, R, P, Err, A8] =
    (flow1 <&> flow2 <&> flow3 <&> flow4 <&> flow5 <&> flow6 <&> flow7).mapOutputChannels {
      case StepOutputs(((((((sa, sb), sc), sd), se), sf), sg), ((((((a, b), c), d), e), f), g)) =>
        val outsA = StepOutputs(state = sa, value = a)
        val outsB = StepOutputs(state = sb, value = b)
        val outsC = StepOutputs(state = sc, value = c)
        val outsD = StepOutputs(state = sd, value = d)
        val outsE = StepOutputs(state = se, value = e)
        val outsF = StepOutputs(state = sf, value = f)
        val outsG = StepOutputs(state = sg, value = g)
        func(outsA, outsB, outsC, outsD, outsE, outsF, outsG)
    }

  def mapParN[S0, R, P, Err, S1, A1, S2, A2, S3, A3, S4, A4, S5, A5, S6, A6, S7, A7, S8, A8, S9, A9](
    flow1: Stage[S0, S1, R, P, Err, A1],
    flow2: Stage[S0, S2, R, P, Err, A2],
    flow3: Stage[S0, S3, R, P, Err, A3],
    flow4: Stage[S0, S4, R, P, Err, A4],
    flow5: Stage[S0, S5, R, P, Err, A5],
    flow6: Stage[S0, S6, R, P, Err, A6],
    flow7: Stage[S0, S7, R, P, Err, A7],
    flow8: Stage[S0, S8, R, P, Err, A8]
  )(
    func: (
      StepOutputs[S1, A1],
      StepOutputs[S2, A2],
      StepOutputs[S3, A3],
      StepOutputs[S4, A4],
      StepOutputs[S5, A5],
      StepOutputs[S6, A6],
      StepOutputs[S7, A7],
      StepOutputs[S8, A8]
    ) => StepOutputs[S9, A9]
  ): Stage[S0, S9, R, P, Err, A9] =
    (flow1 <&> flow2 <&> flow3 <&> flow4 <&> flow5 <&> flow6 <&> flow7 <&> flow8).mapOutputChannels {
      case StepOutputs((((((((sa, sb), sc), sd), se), sf), sg), sh), (((((((a, b), c), d), e), f), g), h)) =>
        val outsA = StepOutputs(state = sa, value = a)
        val outsB = StepOutputs(state = sb, value = b)
        val outsC = StepOutputs(state = sc, value = c)
        val outsD = StepOutputs(state = sd, value = d)
        val outsE = StepOutputs(state = se, value = e)
        val outsF = StepOutputs(state = sf, value = f)
        val outsG = StepOutputs(state = sg, value = g)
        val outsH = StepOutputs(state = sh, value = h)
        func(outsA, outsB, outsC, outsD, outsE, outsF, outsG, outsH)
    }

  def modify[StateIn, StateOut, Output](
    func: StateIn => (StateOut, Output)
  ): Stage[StateIn, StateOut, Any, Any, Nothing, Output] =
    context[Any, StateIn, Any].flatMap { ctx =>
      val (stateOut, output) = func(ctx.inputs.state)
      Stage.succeedWith(value = output, state = stateOut)
    }

  def name[StateIn, StateOut, Env, Params, Err, Value](name: String)(
    step: Stage[StateIn, StateOut, Env, Params, Err, Value]
  ): Stage[StateIn, StateOut, Env, Params, Err, Value] =
    step.named(name)

  /**
   * A step that returns the given parameters.
   */
  def parameters[P]: Stage[Any, Any, BaseEnv, P, Nothing, P] =
    new Stage[Any, Any, BaseEnv, P, Nothing, P](ZIO.fromFunction(ctx => ctx.toOutputs))

  /**
   * Constructs a step that sets the state to the specified value.
   */
  def set[S](s: S): Stage[Any, S, Any, Any, Nothing, Any] =
    modify((_: Any) => (s, ()))

  /**
   * Returns a step that models success with the specified value.
   */
  def succeed[Value](value: => Value): Stage[Any, Any, Any, Any, Nothing, Value] =
    Stage(
      ZIO.accessM[StageContext[Any, Any, Any]](ctx => ZIO.succeed(StepOutputs(state = ctx.inputs.state, value = value)))
    )

  def succeedWith[State, Value](state: => State, value: => Value): Stage[Any, State, Any, Any, Nothing, Value] =
    Stage(ZIO.succeed(StepOutputs(state = state, value = value)))

}
