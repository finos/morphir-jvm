package morphir.flowz

import zio._

import scala.util.Try

/**
 * A flow describes an operation which may have some state and input operations.
 */
final case class Flow[-StateIn, +StateOut, -Env, -Params, +Err, +Output](
  private val effect: ZIO[FlowContext[Env, StateIn, Params], Err, OutputChannels[StateOut, Output]]
) { self =>

  /**
   * Connect this flow to the given flow. By connecting the output state of this flow to the input state of the other flow
   * and by connecting the output value of this flow to the input of the other.
   */
  def >>>[SOut2, Env1 <: Env, Err1 >: Err, Output2](
    that: Flow[StateOut, SOut2, Env1, Output, Err1, Output2]
  ): Flow[StateIn, SOut2, Env1, Params, Err1, Output2] =
    self andThen that

  def <*>[StateIn1 <: StateIn, Env1 <: Env, In1 <: Params, Err1 >: Err, StateOut2, Output2](
    that: Flow[StateIn1, StateOut2, Env1, In1, Err1, Output2]
  ): Flow[StateIn1, (StateOut, StateOut2), Env1, In1, Err1, (Output, Output2)] = self zip that

  /**
   * Adapts the input provided to the flow using the provided function.
   */
  def adaptParameters[Input0](func: Input0 => Params): Flow[StateIn, StateOut, Env, Input0, Err, Output] =
    Flow[StateIn, StateOut, Env, Input0, Err, Output](self.effect.provideSome { ctx =>
      ctx.copy(inputs = ctx.inputs.copy(params = func(ctx.inputs.params)))
    })

  def andThen[SOut2, Env1 <: Env, Err1 >: Err, Output2](
    that: Flow[StateOut, SOut2, Env1, Output, Err1, Output2]
  ): Flow[StateIn, SOut2, Env1, Params, Err1, Output2] =
    Flow(ZIO.environment[FlowContext[Env1, StateIn, Params]].flatMap { ctx =>
      self.effect.flatMap(out => that.effect.provide(ctx.updateInputs(out)))
    })

  def andThenEffect[Err1 >: Err, StateOut2, Output2](
    thatEffect: ZIO[Output, Err1, OutputChannels[StateOut2, Output2]]
  ): Flow[StateIn, StateOut2, Env, Params, Err1, Output2] =
    Flow((self.effect.map(out => out.value) andThen thatEffect))

  /**
   * Maps the success value of this flow to the specified constant value.
   */
  def as[Out2](out: => Out2): Flow[StateIn, StateOut, Env, Params, Err, Out2] = self.map(_ => out)

  def flatMap[S, Env1 <: Env, In1 <: Params, Err1 >: Err, Out1](
    func: Output => Flow[StateOut, S, Env1, In1, Err1, Out1]
  ): Flow[StateIn, S, Env1, In1, Err1, Out1] =
    Flow(ZIO.environment[FlowContext[Env1, StateIn, In1]].flatMap { ctx =>
      self.effect.flatMap(out => func(out.value).effect.provide(ctx.updateState(out.state)))
    })

  def flipOutputs: Flow[StateIn, Output, Env, Params, Err, StateOut] =
    self.mapOutputs { case (state, value) => (value, state) }

  def map[Out2](fn: Output => Out2): Flow[StateIn, StateOut, Env, Params, Err, Out2] = Flow(
    self.effect.map(success => success.map(fn))
  )

  def mapEffect[Out2](fn: Output => Out2)(implicit ev: Err <:< Throwable): RFlow[StateIn, StateOut, Env, Params, Out2] =
    Flow(self.effect.mapEffect(success => success.map(fn)))

  def mapError[Err2](onError: Err => Err2): Flow[StateIn, StateOut, Env, Params, Err2, Output] =
    Flow(self.effect.mapError(onError))

  def mapOutputs[StateOut2, Output2](
    func: (StateOut, Output) => (StateOut2, Output2)
  ): Flow[StateIn, StateOut2, Env, Params, Err, Output2] =
    Flow(self.effect.map(out => OutputChannels.fromTuple(func(out.state, out.value))))

  def mapState[SOut2](fn: StateOut => SOut2): Flow[StateIn, SOut2, Env, Params, Err, Output] = Flow(
    self.effect.map(success => success.mapState(fn))
  )

  def shiftStateToOutput: Flow[StateIn, Unit, Env, Params, Err, (StateOut, Output)] =
    Flow(effect.map(success => success.toFlowValueLeft))

  def run(implicit
    evAnyInput: Any <:< Params,
    evAnyState: Any <:< StateIn
  ): ZIO[Env, Err, OutputChannels[StateOut, Output]] =
    self.effect.provideSome[Env](env => FlowContext(environment = env, params = (), state = ()))

  def run(input: Params)(implicit evAnyState: Unit <:< StateIn): ZIO[Env, Err, OutputChannels[StateOut, Output]] =
    self.effect.provideSome[Env](env => FlowContext(environment = env, params = input, state = ()))

  def run(input: Params, initialState: StateIn): ZIO[Env, Err, OutputChannels[StateOut, Output]] =
    self.effect.provideSome[Env](env => FlowContext(environment = env, params = input, state = initialState))

  /**
   * Maps the output state value of this flow to the specified constant value.
   */
  def stateAs[StateOut2](stateOut: => StateOut2): Flow[StateIn, StateOut2, Env, Params, Err, Output] =
    self.mapState(_ => stateOut)

  def zip[StateIn1 <: StateIn, Env1 <: Env, In1 <: Params, Err1 >: Err, StateOut2, Output2](
    that: Flow[StateIn1, StateOut2, Env1, In1, Err1, Output2]
  ): Flow[StateIn1, (StateOut, StateOut2), Env1, In1, Err1, (Output, Output2)] =
    Flow((self.effect zip that.effect).map { case (left, right) => left zip right })
}

object Flow extends FlowCompanion with AnyEnvFlowCompanion {

  def apply[StateIn, Params, StateOut, Out](
    func: (StateIn, Params) => (StateOut, Out)
  ): TaskFlow[StateIn, StateOut, Params, Out] =
    Flow(ZIO.environment[FlowContext.having.AnyEnv[StateIn, Params]].mapEffect { ctx =>
      val (state, value) = func(ctx.inputs.state, ctx.inputs.params)
      OutputChannels(state = state, value = value)
    })
}

private[flowz] trait FlowCompanion {

  def accepting[State, Params]: Stage[State, State, Params, Params] =
    Flow(ZIO.environment[FlowContext.having.AnyEnv[State, Params]].map(_.inputs.toOutputs))

  def acceptingEnv[R]: RStep[R, Any, R] =
    Flow(ZIO.environment[FlowContext.having.Environment[R]].map { ctx =>
      OutputChannels.fromValue(ctx.environment)
    })

  def acceptingState[S]: Flow[S, S, Any, Any, Nothing, S] =
    Flow(ZIO.environment[FlowContext.having.InputState[S]].map { ctx =>
      OutputChannels(ctx.inputs.state, ctx.inputs.state)
    })

  def context[Env, StateIn, Params]: URFlow[StateIn, Unit, Env, Params, FlowContext[Env, StateIn, Params]] =
    Flow(ZIO.environment[FlowContext[Env, StateIn, Params]].map(OutputChannels.fromValue(_)))

  def effect[Value](value: => Value): TaskStep[Any, Value] =
    Flow(ZIO.environment[FlowContext.having.AnyInputs].mapEffect(_ => OutputChannels.fromValue(value)))

  def environment[Env]: RStep[Env, Any, Env] =
    Flow(ZIO.environment[FlowContext.having.Environment[Env]].map(ctx => FlowValue.fromValue(ctx.environment)))

  def fail[Err](error: Err): Flow[Any, Nothing, Any, Any, Err, Nothing] =
    Flow(ZIO.environment[FlowContext.having.AnyInputs] *> ZIO.fail(error))

  def fromEffect[R, E, A](effect: ZIO[R, E, A]): Step[R, Any, E, A] =
    Flow(
      ZIO
        .environment[FlowContext.having.Environment[R]]
        .flatMap(ctx => effect.map(OutputChannels.fromValue(_)).provide(ctx.environment))
    )

  def fromFunction[In, Out](func: In => Out): Step[Any, In, Nothing, Out] =
    Flow(ZIO.environment[FlowContext.having.Parameters[In]].map { ctx =>
      OutputChannels(value = func(ctx.inputs.params), state = ())
    })

  def fromEither[Err, Value](value: Either[Err, Value]): Step[Any, Any, Err, Value] =
    Flow(for {
      _     <- ZIO.environment[FlowContext.having.AnyInputs]
      value <- ZIO.fromEither(value)
    } yield OutputChannels.fromValue(value))

  def fromOption[Value](value: => Option[Value]): Step[Any, Any, Option[Nothing], Value] =
    Flow(for {
      _     <- ZIO.environment[FlowContext.having.AnyInputs]
      value <- ZIO.fromOption(value)
    } yield OutputChannels.fromValue(value))

  def fromTry[Value](value: => Try[Value]): TaskStep[Any, Value] =
    Flow(for {
      _     <- ZIO.environment[FlowContext.having.AnyInputs]
      value <- ZIO.fromTry(value)
    } yield OutputChannels.fromValue(value))

  def get[State]: Flow[State, State, Any, Any, Nothing, State] =
    modify[State, State, State](s => (s, s))

  def modify[StateIn, StateOut, Output](
    func: StateIn => (StateOut, Output)
  ): Flow[StateIn, StateOut, Any, Any, Nothing, Output] =
    context[Any, StateIn, Any].flatMap { ctx =>
      val (stateOut, output) = func(ctx.inputs.state)
      Flow.succeed(value = output, state = stateOut)
    }

  /**
   * Returns a flow with the empty value.
   */
  val none: UStep[Any, Option[Nothing]] =
    Flow(ZIO.environment[FlowContext.having.AnyInputs].as(OutputChannels.none))

  /**
   * A flow that succeeds with a unit value.
   */
  val unit: UStep[Any, Unit] =
    Flow(ZIO.environment[FlowContext.having.AnyInputs].as(OutputChannels.unit))

  def update[StateIn, StateOut](func: StateIn => StateOut): Flow[StateIn, StateOut, Any, Any, Nothing, Unit] =
    Flow(ZIO.environment[FlowContext.having.InputState[StateIn]].map { ctx =>
      OutputChannels.fromState(func(ctx.inputs.state))
    })

  def set[S](state: S): Flow[Any, S, Any, Any, Nothing, Unit] =
    modify { _: Any => (state, ()) }

  def stage[StateIn, Params, StateOut, Out](
    func: (StateIn, Params) => (StateOut, Out)
  ): Stage[StateIn, StateOut, Params, Out] =
    Flow(ZIO.environment[FlowContext.having.AnyEnv[StateIn, Params]].map { ctx =>
      val (state, value) = func(ctx.inputs.state, ctx.inputs.params)
      OutputChannels(state = state, value = value)
    })

  def state[S]: Stage[S, S, Any, S] =
    context[Any, S, Any].mapOutputs { case (_, ctx) => (ctx.inputs.state, ctx.inputs.state) }

  def stateful[StateIn, Params, StateOut, Out](
    func: (StateIn, Params) => (StateOut, Out)
  ): Flow[StateIn, StateOut, Any, Params, Throwable, Out] =
    Flow(ZIO.environment[FlowContext.having.AnyEnv[StateIn, Params]].mapEffect { ctx =>
      val (state, value) = func(ctx.inputs.state, ctx.inputs.params)
      OutputChannels(state = state, value = value)
    })

  def succeed[A](value: => A): UStep[Any, A] =
    Flow(ZIO.environment[FlowContext.having.AnyInputs].as(OutputChannels.fromValue(value)))

  def succeed[Value, State](value: => Value, state: => State): SrcFlow[State, Value] =
    Flow(ZIO.environment[FlowContext.having.AnyInputs].as(OutputChannels(value = value, state = state)))

  def task[In, Out](func: In => Out): TaskStep[In, Out] =
    Flow(ZIO.environment[FlowContext.having.Parameters[In]].mapEffect { ctx =>
      OutputChannels(value = func(ctx.inputs.params), state = ())
    })
}

private[flowz] trait AnyEnvFlowCompanion {

  /**
   * A step that returns the given parameters.
   */
  def parameters[P]: UStep[P, P] =
    Flow.context[Any, Any, P].map { ctx =>
      ctx.inputs.params
    }
}
