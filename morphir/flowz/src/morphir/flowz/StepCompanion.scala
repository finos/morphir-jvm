package morphir.flowz

import zio._

abstract class StepCompanion[-BaseEnv] {

  /**
   * A step that uses its parameters to build another Step.
   */
  def accessParametersM[SIn, SOut, R, P, E, A](func: P => Step[SIn, SOut, R, P, E, A]): Step[SIn, SOut, R, P, E, A] =
    Step(ZIO.accessM[StepContext[R, SIn, P]](ctx => func(ctx.inputs.params).effect))

  /** Defines a step that does not rely on state. */
  def act[Params, Out](f: Params => Out): Activity[Any, Params, Throwable, Out] =
    Step(
      ZIO
        .environment[StepContext.having.Parameters[Params]]
        .mapEffect(ctx => StepOutputs.assignBoth(f(ctx.inputs.params)))
    )

  /** Defines a step that does not rely on state. */
  def act[Params, Out](name: String)(f: Params => Out): Activity[Any, Params, Throwable, Out] =
    Step(
      ZIO
        .environment[StepContext.having.Parameters[Params]]
        .mapEffect(ctx => StepOutputs.assignBoth(f(ctx.inputs.params))),
      name = Option(name)
    )

  /** Defines a step that does not rely on state. */
  def act[Params, Out](name: String, description: String)(f: Params => Out): Activity[Any, Params, Throwable, Out] =
    Step(
      ZIO
        .environment[StepContext.having.Parameters[Params]]
        .mapEffect(ctx => StepOutputs.assignBoth(f(ctx.inputs.params))),
      name = Option(name),
      description = Option(description)
    )

  /** Defines a step that performs some effect which does not rely on state. */
  def activity[Env, Params, Err, Value](func: Params => ZIO[Env, Err, Value]): Activity[Env, Params, Err, Value] =
    Step(ZIO.accessM[StepContext[Env, Any, Params]] { ctx =>
      func(ctx.inputs.params).map(value => StepOutputs(state = value, value = value)).provide(ctx.environment)
    })

  /** Defines a step that performs some effect which does not rely on state. */
  def activity[Env, Params, Err, Value](name: String)(
    func: Params => ZIO[Env, Err, Value]
  ): Activity[Env, Params, Err, Value] =
    Step(
      ZIO.accessM[StepContext[Env, Any, Params]] { ctx =>
        func(ctx.inputs.params).map(value => StepOutputs(state = value, value = value)).provide(ctx.environment)
      },
      name = Option(name)
    )

  def activity[Env, Params, Err, Value](name: String, description: String)(
    func: Params => ZIO[Env, Err, Value]
  ): Activity[Env, Params, Err, Value] =
    Step(
      ZIO.accessM[StepContext[Env, Any, Params]] { ctx =>
        func(ctx.inputs.params).map(value => StepOutputs(state = value, value = value)).provide(ctx.environment)
      },
      name = Option(name),
      description = Option(description)
    )

  def context[Env, StateIn, Params]: Step[StateIn, StateIn, Env, Params, Nothing, StepContext[Env, StateIn, Params]] =
    Step(
      ZIO
        .environment[StepContext[Env, StateIn, Params]]
        .map(ctx => StepOutputs(value = ctx, state = ctx.inputs.state))
    )

  def describe[StateIn, StateOut, Env, Params, Err, Value](description: String)(
    step: Step[StateIn, StateOut, Env, Params, Err, Value]
  ): Step[StateIn, StateOut, Env, Params, Err, Value] =
    step.describe(description)

  def effect[Value](value: => Value): TaskStep[Any, Value] =
    Step(ZIO.environment[StepContext.having.AnyInputs].mapEffect(_ => StepOutputs.fromValue(value)))

  def environment[Env <: BaseEnv]: Step[Any, Env, Env, Any, Nothing, Env] =
    Step(ZIO.environment[StepContext.having.Environment[Env]].map(ctx => StepOutputs.setBoth(ctx.environment)))

  def fail[Err](error: Err): Step[Any, Nothing, BaseEnv, Any, Err, Nothing] =
    Step(ZIO.environment[StepContext.having.AnyInputs] *> ZIO.fail(error))

  def fromEffect[R, E, A](effect: ZIO[R, E, A]): Step[Any, Unit, R, Any, E, A] =
    Step(
      ZIO
        .environment[StepContext[R, Any, Any]]
        .flatMap(ctx => effect.map(StepOutputs.fromValue(_)).provide(ctx.environment))
    )

  def fromEffect[P, R, E, A](func: P => ZIO[R, E, A]): Step[Any, Unit, R, P, E, A] =
    Step(
      ZIO
        .environment[StepContext[R, Any, P]]
        .flatMap(ctx => func(ctx.inputs.params).map(StepOutputs.fromValue(_)).provide(ctx.environment))
    )

  /**
   * Get the state.
   */
  def get[State]: Step[State, State, Any, Any, Nothing, State] =
    modify[State, State, State](s => (s, s))

  def mapN[S0, R, P, Err, SA, A, SB, B, SC, C](flowA: Step[S0, SA, R, P, Err, A], flowB: Step[S0, SB, R, P, Err, B])(
    f: (StepOutputs[SA, A], StepOutputs[SB, B]) => StepOutputs[SC, C]
  ): Step[S0, SC, R, P, Err, C] =
    flowA.zipWith(flowB)(f)

  def mapParN[S0, R, P, Err, SA, A, SB, B, SC, C](
    flowA: Step[S0, SA, R, P, Err, A],
    flowB: Step[S0, SB, R, P, Err, B]
  )(
    f: (StepOutputs[SA, A], StepOutputs[SB, B]) => StepOutputs[SC, C]
  ): Step[S0, SC, R, P, Err, C] =
    flowA.zipWithPar(flowB)(f)

  def mapParN[S0, R, P, Err, SA, A, SB, B, SC, C, SD, D](
    flowA: Step[S0, SA, R, P, Err, A],
    flowB: Step[S0, SB, R, P, Err, B],
    flowC: Step[S0, SC, R, P, Err, C]
  )(
    f: (StepOutputs[SA, A], StepOutputs[SB, B], StepOutputs[SC, C]) => StepOutputs[SD, D]
  ): Step[S0, SD, R, P, Err, D] =
    (flowA <&> flowB <&> flowC).mapOutputChannels { case StepOutputs(((sa, sb), sc), ((a, b), c)) =>
      val outsA = StepOutputs(state = sa, value = a)
      val outsB = StepOutputs(state = sb, value = b)
      val outsC = StepOutputs(state = sc, value = c)
      f(outsA, outsB, outsC)
    }

  def mapParN[S0, R, P, Err, SA, A, SB, B, SC, C, SD, D, SF, F](
    flowA: Step[S0, SA, R, P, Err, A],
    flowB: Step[S0, SB, R, P, Err, B],
    flowC: Step[S0, SC, R, P, Err, C],
    flowD: Step[S0, SD, R, P, Err, D]
  )(
    f: (StepOutputs[SA, A], StepOutputs[SB, B], StepOutputs[SC, C], StepOutputs[SD, D]) => StepOutputs[SF, F]
  ): Step[S0, SF, R, P, Err, F] =
    (flowA <&> flowB <&> flowC <&> flowD).mapOutputChannels {
      case StepOutputs((((sa, sb), sc), sd), (((a, b), c), d)) =>
        val outsA = StepOutputs(state = sa, value = a)
        val outsB = StepOutputs(state = sb, value = b)
        val outsC = StepOutputs(state = sc, value = c)
        val outsD = StepOutputs(state = sd, value = d)
        f(outsA, outsB, outsC, outsD)
    }

  def mapParN[S0, R, P, Err, S1, A1, S2, A2, S3, A3, S4, A4, S5, A5, S6, A6](
    flow1: Step[S0, S1, R, P, Err, A1],
    flow2: Step[S0, S2, R, P, Err, A2],
    flow3: Step[S0, S3, R, P, Err, A3],
    flow4: Step[S0, S4, R, P, Err, A4],
    flow5: Step[S0, S5, R, P, Err, A5]
  )(
    f: (
      StepOutputs[S1, A1],
      StepOutputs[S2, A2],
      StepOutputs[S3, A3],
      StepOutputs[S4, A4],
      StepOutputs[S5, A5]
    ) => StepOutputs[S6, A6]
  ): Step[S0, S6, R, P, Err, A6] =
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
    flow1: Step[S0, S1, R, P, Err, A1],
    flow2: Step[S0, S2, R, P, Err, A2],
    flow3: Step[S0, S3, R, P, Err, A3],
    flow4: Step[S0, S4, R, P, Err, A4],
    flow5: Step[S0, S5, R, P, Err, A5],
    flow6: Step[S0, S6, R, P, Err, A6]
  )(
    func: (
      StepOutputs[S1, A1],
      StepOutputs[S2, A2],
      StepOutputs[S3, A3],
      StepOutputs[S4, A4],
      StepOutputs[S5, A5],
      StepOutputs[S6, A6]
    ) => StepOutputs[S7, A7]
  ): Step[S0, S7, R, P, Err, A7] =
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
    flow1: Step[S0, S1, R, P, Err, A1],
    flow2: Step[S0, S2, R, P, Err, A2],
    flow3: Step[S0, S3, R, P, Err, A3],
    flow4: Step[S0, S4, R, P, Err, A4],
    flow5: Step[S0, S5, R, P, Err, A5],
    flow6: Step[S0, S6, R, P, Err, A6],
    flow7: Step[S0, S7, R, P, Err, A7]
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
  ): Step[S0, S8, R, P, Err, A8] =
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
    flow1: Step[S0, S1, R, P, Err, A1],
    flow2: Step[S0, S2, R, P, Err, A2],
    flow3: Step[S0, S3, R, P, Err, A3],
    flow4: Step[S0, S4, R, P, Err, A4],
    flow5: Step[S0, S5, R, P, Err, A5],
    flow6: Step[S0, S6, R, P, Err, A6],
    flow7: Step[S0, S7, R, P, Err, A7],
    flow8: Step[S0, S8, R, P, Err, A8]
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
  ): Step[S0, S9, R, P, Err, A9] =
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
  ): Step[StateIn, StateOut, Any, Any, Nothing, Output] =
    context[Any, StateIn, Any].flatMap { ctx =>
      val (stateOut, output) = func(ctx.inputs.state)
      Step.succeed(value = output, state = stateOut)
    }

  def name[StateIn, StateOut, Env, Params, Err, Value](name: String)(
    step: Step[StateIn, StateOut, Env, Params, Err, Value]
  ): Step[StateIn, StateOut, Env, Params, Err, Value] =
    step.named(name)

  /**
   * A step that returns the given parameters.
   */
  def parameters[P]: Step[Any, Any, BaseEnv, P, Nothing, P] =
    new Step[Any, Any, BaseEnv, P, Nothing, P](ZIO.fromFunction(ctx => ctx.toOutputs))

  /**
   * Returns a step that models success with the specified value.
   */
  def succeed[Value](value: => Value): Step[Any, Any, Any, Any, Nothing, Value] =
    Step(
      ZIO.accessM[StepContext[Any, Any, Any]](ctx => ZIO.succeed(StepOutputs(state = ctx.inputs.state, value = value)))
    )

  def succeed[State, Value](state: => State, value: => Value): Step[Any, State, Any, Any, Nothing, Value] =
    Step(ZIO.succeed(StepOutputs(state = state, value = value)))

}
