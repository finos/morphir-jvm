package morphir.flowz

import zio._

final case class Flow[-StateIn, +StateOut, -Env, -Params, +Err, +Output](
  private val effect: ZIO[FlowContext[Env, StateIn, Params], Err, OutputChannels[StateOut, Output]]
) { self =>

  def >>>[SOut2, Env1 <: Env, Err1 >: Err, Output2](
    that: Flow[StateOut, SOut2, Env1, Output, Err1, Output2]
  ): Flow[StateIn, SOut2, Env1, Params, Err1, Output2] =
    self andThen that

  /**
   * Adapts the input provided to the flow using the provided function.
   */
  def adaptInput[Input0](func: Input0 => Params): Flow[StateIn, StateOut, Env, Input0, Err, Output] =
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

  def map[Out2](fn: Output => Out2): Flow[StateIn, StateOut, Env, Params, Err, Out2] = Flow(
    self.effect.map(success => success.map(fn))
  )

  def mapEffect[Out2](fn: Output => Out2)(implicit ev: Err <:< Throwable): RFlow[StateIn, StateOut, Env, Params, Out2] =
    Flow(self.effect.mapEffect(success => success.map(fn)))

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

object Flow extends FlowCompanion with AnyEnvFlowCompanion {}

private[flowz] trait FlowCompanion {

  def acceptingEnv[R]: RStep[R, Any, R] =
    Flow(ZIO.environment[FlowContext.having.EnvOf[R]].map { ctx =>
      OutputChannels.fromValue(ctx.environment)
    })

  def acceptingState[S]: Flow[S, S, Any, Any, Nothing, S] =
    Flow(ZIO.environment[FlowContext.having.StateOf[S]].map { ctx =>
      OutputChannels(ctx.inputs.state, ctx.inputs.state)
    })

  def effect[In, Out](func: In => Out): TaskStep[In, Out] =
    Flow(ZIO.environment[FlowContext.having.ParamsOf[In]].mapEffect { ctx =>
      OutputChannels(value = func(ctx.inputs.params), state = ())
    })

  def environment[Env]: RStep[Env, Any, Env] =
    Flow(ZIO.environment[FlowContext.having.EnvOf[Env]].map(ctx => FlowValue.fromValue(ctx.environment)))

  def fromEffect[R, E, A](effect: ZIO[R, E, A]) = ???
  //Flow.environment[R].flatMap()

  def fromFunction[In, Out](func: In => Out): Step[Any, In, Nothing, Out] =
    Flow(ZIO.environment[FlowContext.having.ParamsOf[In]].map { ctx =>
      OutputChannels(value = func(ctx.inputs.params), state = ())
    })

  def get[State]: Flow[State, State, Any, Any, Nothing, State] =
    modify[State, State, State](s => (s, s))

  def context[Env, StateIn, In]: URFlow[StateIn, Unit, Env, In, FlowContext[Env, StateIn, In]] =
    Flow(ZIO.environment[FlowContext[Env, StateIn, In]].map(OutputChannels.fromValue(_)))

  def modify[StateIn, StateOut, Output](
    func: StateIn => (StateOut, Output)
  ): Flow[StateIn, StateOut, Any, Any, Nothing, Output] =
    context[Any, StateIn, Any].flatMap { ctx =>
      val (stateOut, output) = func(ctx.inputs.state)
      Flow.succeed(output = output, state = stateOut)
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
    Flow(ZIO.environment[FlowContext.having.StateOf[StateIn]].map { ctx =>
      OutputChannels.fromState(func(ctx.inputs.state))
    })

  def set[S](state: S): Flow[Any, S, Any, Any, Nothing, Unit] =
    modify { _: Any => (state, ()) }

  def succeed[A](value: => A): UStep[Any, A] =
    Flow(ZIO.environment[FlowContext.having.AnyInputs].as(OutputChannels.fromValue(value)))

  def succeed[Output, State](output: Output, state: State): SrcFlow[State, Output] =
    Flow(ZIO.environment[FlowContext.having.AnyInputs].as(OutputChannels(value = output, state = state)))

  def fail[Err](error: Err): Flow[Any, Nothing, Any, Any, Err, Nothing] =
    Flow(ZIO.environment[FlowContext.having.AnyInputs] *> ZIO.fail(error))
}

private[flowz] trait AnyEnvFlowCompanion {

  /**
   * A step that returns the given parameters.
   */
  def parameters[In]: TaskStep[In, In] =
    Flow.context[Any, Any, In].mapEffect { ctx =>
      ctx.inputs.params
    }
}
