package morphir.flowz

import zio._

final case class Flow[-StateIn, +StateOut, -Env, -Input, +Err, +Output](
  private val effect: ZIO[(Env, Input, StateIn), Err, FlowOutputs[StateOut, Output]]
) { self =>

  def >>>[SOut2, Env1 <: Env, Err1 >: Err, Output2](
    that: Flow[StateOut, SOut2, Env1, Output, Err1, Output2]
  ): Flow[StateIn, SOut2, Env1, Input, Err1, Output2] =
    self andThen that

  /**
   * Adapts the input provided to the flow using the provided function.
   */
  def adaptInput[Input0](func: Input0 => Input): Flow[StateIn, StateOut, Env, Input0, Err, Output] =
    Flow[StateIn, StateOut, Env, Input0, Err, Output](self.effect.provideSome { case (env, input0, stateIn) =>
      (env, func(input0), stateIn)
    })

  def andThen[SOut2, Env1 <: Env, Err1 >: Err, Output2](
    that: Flow[StateOut, SOut2, Env1, Output, Err1, Output2]
  ): Flow[StateIn, SOut2, Env1, Input, Err1, Output2] =
    Flow(ZIO.environment[(Env1, Input, StateIn)].flatMap { case (env, _, _) =>
      self.effect.flatMap(out => that.effect.provide((env, out.output, out.state)))
    })

  /**
   * Maps the success value of this flow to the specified constant value.
   */
  def as[Out2](out: => Out2): Flow[StateIn, StateOut, Env, Input, Err, Out2] = self.map(_ => out)

  def flatMap[S, Env1 <: Env, In1 <: Input, Err1 >: Err, Out1](
    func: Output => Flow[StateOut, S, Env1, In1, Err1, Out1]
  ): Flow[StateIn, S, Env1, In1, Err1, Out1] =
    Flow(ZIO.environment[(Env1, In1, StateIn)].flatMap { case (env1, in1, _) =>
      self.effect.flatMap(success => func(success.output).effect.provide((env1, in1, success.state)))
    })

  def map[Out2](fn: Output => Out2): Flow[StateIn, StateOut, Env, Input, Err, Out2] = Flow(
    self.effect.map(success => success.map(fn))
  )

  def mapEffect[Out2](fn: Output => Out2)(implicit ev: Err <:< Throwable): RFlow[StateIn, StateOut, Env, Input, Out2] =
    Flow(self.effect.mapEffect(success => success.map(fn)))

  def mapOutputs[StateOut2, Output2](
    func: (StateOut, Output) => (StateOut2, Output2)
  ): Flow[StateIn, StateOut2, Env, Input, Err, Output2] =
    Flow(self.effect.map(out => FlowOutputs.fromTuple(func(out.state, out.output))))

  def mapState[SOut2](fn: StateOut => SOut2): Flow[StateIn, SOut2, Env, Input, Err, Output] = Flow(
    self.effect.map(success => success.mapState(fn))
  )

  def shiftStateToOutput: Flow[StateIn, Unit, Env, Input, Err, (StateOut, Output)] =
    Flow(effect.map(success => success.toFlowOutputLeft))

  def run(implicit
    evAnyInput: Any <:< Input,
    evAnyState: Any <:< StateIn
  ): ZIO[Env, Err, FlowOutputs[StateOut, Output]] =
    self.effect.provideSome[Env](env => (env, (), ()))

  def run(input: Input)(implicit evAnyState: Unit <:< StateIn): ZIO[Env, Err, FlowOutputs[StateOut, Output]] =
    self.effect.provideSome[Env](env => (env, input, ()))

  def run(input: Input, initialState: StateIn): ZIO[Env, Err, FlowOutputs[StateOut, Output]] =
    self.effect.provideSome[Env](env => (env, input, initialState))

  /**
   * Maps the output state value of this flow to the specified constant value.
   */
  def stateAs[StateOut2](stateOut: => StateOut2): Flow[StateIn, StateOut2, Env, Input, Err, Output] =
    self.mapState(_ => stateOut)

  def zip[StateIn1 <: StateIn, Env1 <: Env, In1 <: Input, Err1 >: Err, StateOut2, Output2](
    that: Flow[StateIn1, StateOut2, Env1, In1, Err1, Output2]
  ): Flow[StateIn1, (StateOut, StateOut2), Env1, In1, Err1, (Output, Output2)] =
    Flow((self.effect zip that.effect).map { case (left, right) => left zip right })
}

object Flow extends FlowCompanion {}

private[flowz] trait FlowCompanion {

  def fromEffectful[In, Out](func: In => Out): TaskStep[In, Out] =
    Flow(ZIO.environment[(Any, In, Any)].mapEffect { case (_, in, _) =>
      FlowOutputs(output = func(in), state = ())
    })

  def fromFunction[In, Out](func: In => Out): Step[Any, In, Nothing, Out] =
    Flow(ZIO.environment[(Any, In, Any)].map { case (_, in, _) =>
      FlowOutputs(output = func(in), state = ())
    })

  def get[State]: Flow[State, State, Any, Any, Nothing, State] =
    modify[State, State, State](s => (s, s))

  def inputs[Env, In, StateIn]: URFlow[StateIn, Unit, Env, In, (Env, In, StateIn)] =
    Flow(ZIO.environment[(Env, In, StateIn)].map(FlowOutputs.fromOutput(_)))

  def modify[StateIn, StateOut, Output](
    func: StateIn => (StateOut, Output)
  ): Flow[StateIn, StateOut, Any, Any, Nothing, Output] =
    inputs[Any, Any, StateIn].flatMap { case (_, _, stateIn) =>
      val (stateOut, output) = func(stateIn)
      Flow.succeed(output = output, state = stateOut)
    }

  /**
   * Returns a flow with the empty value.
   */
  val none: UStep[Any, Option[Nothing]] =
    Flow(ZIO.environment[(Any, Any, Any)].as(FlowOutputs.none))

  /**
   * A flow that succeeds with a unit value.
   */
  val unit: UStep[Any, Unit] =
    Flow(ZIO.environment[(Any, Any, Any)].as(FlowOutputs.unit))

  def update[StateIn, StateOut](func: StateIn => StateOut): Flow[StateIn, StateOut, Any, Any, Nothing, Unit] =
    Flow(ZIO.environment[(Any, Any, StateIn)].map { case (_, _, stateIn) =>
      FlowOutputs.fromState(func(stateIn))
    })

  def set[S](state: S): Flow[Any, S, Any, Any, Nothing, Unit] =
    modify { _: Any => (state, ()) }

  def succeed[A](value: => A): UStep[Any, A] =
    Flow(ZIO.environment[(Any, Any, Any)].as(FlowOutputs.fromOutput(value)))

  def succeed[Output, State](output: Output, state: State): SrcFlow[State, Output] =
    Flow(ZIO.environment[(Any, Any, Any)].as(FlowOutputs(output = output, state = state)))

  def fail[Err](error: Err): Flow[Any, Nothing, Any, Any, Err, Nothing] =
    Flow(ZIO.environment[(Any, Any, Any)] *> ZIO.fail(error))
}
