package morphir.flowz

import zio._

abstract class ActCompanion[-BaseEnv] {

  /**
   * A step that uses its parameters to build another Step.
   */
  def accessParametersM[SIn, SOut, R, P, E, A](
    func: P => Act[SIn, SOut, R, P, E, A]
  ): Act[SIn, SOut, R, P, E, A] =
    Act(ZIO.accessM[ActContext[R, SIn, P]](ctx => func(ctx.inputs.params).effect))

  /** Defines a step that does not rely on state. */
  def act[Params, Out](f: Params => Out): Activity[Any, Params, Throwable, Out] =
    Act(
      ZIO
        .environment[ActContext.having.Parameters[Params]]
        .mapEffect(ctx => StepOutputs.assignBoth(f(ctx.inputs.params)))
    )

  /** Defines a step that does not rely on state. */
  def act[Params, Out](name: String)(f: Params => Out): Activity[Any, Params, Throwable, Out] =
    Act(
      ZIO
        .environment[ActContext.having.Parameters[Params]]
        .mapEffect(ctx => StepOutputs.assignBoth(f(ctx.inputs.params))),
      name = Option(name)
    )

  /** Defines a step that does not rely on state. */
  def act[Params, Out](name: String, description: String)(f: Params => Out): Activity[Any, Params, Throwable, Out] =
    Act(
      ZIO
        .environment[ActContext.having.Parameters[Params]]
        .mapEffect(ctx => StepOutputs.assignBoth(f(ctx.inputs.params))),
      name = Option(name),
      description = Option(description)
    )

  /** Defines a step that performs some effect which does not rely on state. */
  def activity[Env, Params, Err, Value](func: Params => ZIO[Env, Err, Value]): Activity[Env, Params, Err, Value] =
    Act(ZIO.accessM[ActContext[Env, Any, Params]] { ctx =>
      func(ctx.inputs.params).map(value => StepOutputs(state = value, value = value)).provide(ctx.environment)
    })

  /** Defines a step that performs some effect which does not rely on state. */
  def activity[Env, Params, Err, Value](name: String)(
    func: Params => ZIO[Env, Err, Value]
  ): Activity[Env, Params, Err, Value] =
    Act(
      ZIO.accessM[ActContext[Env, Any, Params]] { ctx =>
        func(ctx.inputs.params).map(value => StepOutputs(state = value, value = value)).provide(ctx.environment)
      },
      name = Option(name)
    )

  def activity[Env, Params, Err, Value](name: String, description: String)(
    func: Params => ZIO[Env, Err, Value]
  ): Activity[Env, Params, Err, Value] =
    Act(
      ZIO.accessM[ActContext[Env, Any, Params]] { ctx =>
        func(ctx.inputs.params).map(value => StepOutputs(state = value, value = value)).provide(ctx.environment)
      },
      name = Option(name),
      description = Option(description)
    )

  def context[Env, StateIn, Params]: Act[StateIn, StateIn, Env, Params, Nothing, ActContext[Env, StateIn, Params]] =
    Act(
      ZIO
        .environment[ActContext[Env, StateIn, Params]]
        .map(ctx => StepOutputs(value = ctx, state = ctx.inputs.state))
    )

  def describe[StateIn, StateOut, Env, Params, Err, Value](description: String)(
    step: Act[StateIn, StateOut, Env, Params, Err, Value]
  ): Act[StateIn, StateOut, Env, Params, Err, Value] =
    step.describe(description)

  def effect[Value](value: => Value): TaskAct[Any, Value] =
    Act(ZIO.environment[ActContext.having.AnyInputs].mapEffect(_ => StepOutputs.fromValue(value)))

  def environment[Env <: BaseEnv]: Act[Any, Env, Env, Any, Nothing, Env] =
    Act(ZIO.environment[ActContext.having.Environment[Env]].map(ctx => StepOutputs.setBoth(ctx.environment)))

  def fail[Err](error: Err): Act[Any, Nothing, BaseEnv, Any, Err, Nothing] =
    Act(ZIO.environment[ActContext.having.AnyInputs] *> ZIO.fail(error))

  def fromEffect[R, E, A](effect: ZIO[R, E, A]): Act[Any, Any, R, Any, E, A] =
    Act(
      ZIO
        .environment[ActContext[R, Any, Any]]
        .flatMap(ctx => effect.map(ctx.toOutputs(_)).provide(ctx.environment))
    )

  def fromEffect[P, R, E, A](func: P => ZIO[R, E, A]): Act[Any, Unit, R, P, E, A] =
    Act(
      ZIO
        .environment[ActContext[R, Any, P]]
        .flatMap(ctx => func(ctx.inputs.params).map(StepOutputs.fromValue(_)).provide(ctx.environment))
    )

  /**
   * Get the state.
   */
  def get[State]: Act[State, State, Any, Any, Nothing, State] =
    modify[State, State, State](s => (s, s))

  def mapN[S0, R, P, Err, SA, A, SB, B, SC, C](
    flowA: Act[S0, SA, R, P, Err, A],
    flowB: Act[S0, SB, R, P, Err, B]
  )(
    f: (StepOutputs[SA, A], StepOutputs[SB, B]) => StepOutputs[SC, C]
  ): Act[S0, SC, R, P, Err, C] =
    flowA.zipWith(flowB)(f)

  def mapParN[S0, R, P, Err, SA, A, SB, B, SC, C](
    flowA: Act[S0, SA, R, P, Err, A],
    flowB: Act[S0, SB, R, P, Err, B]
  )(
    f: (StepOutputs[SA, A], StepOutputs[SB, B]) => StepOutputs[SC, C]
  ): Act[S0, SC, R, P, Err, C] =
    flowA.zipWithPar(flowB)(f)

  def mapParN[S0, R, P, Err, SA, A, SB, B, SC, C, SD, D](
    flowA: Act[S0, SA, R, P, Err, A],
    flowB: Act[S0, SB, R, P, Err, B],
    flowC: Act[S0, SC, R, P, Err, C]
  )(
    f: (StepOutputs[SA, A], StepOutputs[SB, B], StepOutputs[SC, C]) => StepOutputs[SD, D]
  ): Act[S0, SD, R, P, Err, D] =
    (flowA <&> flowB <&> flowC).mapOutputChannels { case StepOutputs(((sa, sb), sc), ((a, b), c)) =>
      val outsA = StepOutputs(state = sa, value = a)
      val outsB = StepOutputs(state = sb, value = b)
      val outsC = StepOutputs(state = sc, value = c)
      f(outsA, outsB, outsC)
    }

  def mapParN[S0, R, P, Err, SA, A, SB, B, SC, C, SD, D, SF, F](
    flowA: Act[S0, SA, R, P, Err, A],
    flowB: Act[S0, SB, R, P, Err, B],
    flowC: Act[S0, SC, R, P, Err, C],
    flowD: Act[S0, SD, R, P, Err, D]
  )(
    f: (StepOutputs[SA, A], StepOutputs[SB, B], StepOutputs[SC, C], StepOutputs[SD, D]) => StepOutputs[SF, F]
  ): Act[S0, SF, R, P, Err, F] =
    (flowA <&> flowB <&> flowC <&> flowD).mapOutputChannels {
      case StepOutputs((((sa, sb), sc), sd), (((a, b), c), d)) =>
        val outsA = StepOutputs(state = sa, value = a)
        val outsB = StepOutputs(state = sb, value = b)
        val outsC = StepOutputs(state = sc, value = c)
        val outsD = StepOutputs(state = sd, value = d)
        f(outsA, outsB, outsC, outsD)
    }

  def mapParN[S0, R, P, Err, S1, A1, S2, A2, S3, A3, S4, A4, S5, A5, S6, A6](
    flow1: Act[S0, S1, R, P, Err, A1],
    flow2: Act[S0, S2, R, P, Err, A2],
    flow3: Act[S0, S3, R, P, Err, A3],
    flow4: Act[S0, S4, R, P, Err, A4],
    flow5: Act[S0, S5, R, P, Err, A5]
  )(
    f: (
      StepOutputs[S1, A1],
      StepOutputs[S2, A2],
      StepOutputs[S3, A3],
      StepOutputs[S4, A4],
      StepOutputs[S5, A5]
    ) => StepOutputs[S6, A6]
  ): Act[S0, S6, R, P, Err, A6] =
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
    flow1: Act[S0, S1, R, P, Err, A1],
    flow2: Act[S0, S2, R, P, Err, A2],
    flow3: Act[S0, S3, R, P, Err, A3],
    flow4: Act[S0, S4, R, P, Err, A4],
    flow5: Act[S0, S5, R, P, Err, A5],
    flow6: Act[S0, S6, R, P, Err, A6]
  )(
    func: (
      StepOutputs[S1, A1],
      StepOutputs[S2, A2],
      StepOutputs[S3, A3],
      StepOutputs[S4, A4],
      StepOutputs[S5, A5],
      StepOutputs[S6, A6]
    ) => StepOutputs[S7, A7]
  ): Act[S0, S7, R, P, Err, A7] =
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
    flow1: Act[S0, S1, R, P, Err, A1],
    flow2: Act[S0, S2, R, P, Err, A2],
    flow3: Act[S0, S3, R, P, Err, A3],
    flow4: Act[S0, S4, R, P, Err, A4],
    flow5: Act[S0, S5, R, P, Err, A5],
    flow6: Act[S0, S6, R, P, Err, A6],
    flow7: Act[S0, S7, R, P, Err, A7]
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
  ): Act[S0, S8, R, P, Err, A8] =
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
    flow1: Act[S0, S1, R, P, Err, A1],
    flow2: Act[S0, S2, R, P, Err, A2],
    flow3: Act[S0, S3, R, P, Err, A3],
    flow4: Act[S0, S4, R, P, Err, A4],
    flow5: Act[S0, S5, R, P, Err, A5],
    flow6: Act[S0, S6, R, P, Err, A6],
    flow7: Act[S0, S7, R, P, Err, A7],
    flow8: Act[S0, S8, R, P, Err, A8]
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
  ): Act[S0, S9, R, P, Err, A9] =
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
  ): Act[StateIn, StateOut, Any, Any, Nothing, Output] =
    context[Any, StateIn, Any].flatMap { ctx =>
      val (stateOut, output) = func(ctx.inputs.state)
      Act.succeedWith(value = output, state = stateOut)
    }

  def name[StateIn, StateOut, Env, Params, Err, Value](name: String)(
    step: Act[StateIn, StateOut, Env, Params, Err, Value]
  ): Act[StateIn, StateOut, Env, Params, Err, Value] =
    step.named(name)

  /**
   * A step that returns the given parameters.
   */
  def parameters[P]: Act[Any, Any, BaseEnv, P, Nothing, P] =
    new Act[Any, Any, BaseEnv, P, Nothing, P](ZIO.fromFunction(ctx => ctx.toOutputs))

  /**
   * Constructs a step that sets the state to the specified value.
   */
  def set[S](s: S): Act[Any, S, Any, Any, Nothing, Any] =
    modify((_: Any) => (s, ()))

  /**
   * Returns a step that models success with the specified value.
   */
  def succeed[Value](value: => Value): Act[Any, Any, Any, Any, Nothing, Value] =
    Act(
      ZIO.accessM[ActContext[Any, Any, Any]](ctx => ZIO.succeed(StepOutputs(state = ctx.inputs.state, value = value)))
    )

  def succeedWith[State, Value](state: => State, value: => Value): Act[Any, State, Any, Any, Nothing, Value] =
    Act(ZIO.succeed(StepOutputs(state = state, value = value)))

}
