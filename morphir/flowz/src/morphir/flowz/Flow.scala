package morphir.flowz

import zio._

final case class Flow[-StateIn, +StateOut, -Env, -Input, +Err, +Output](
  private val effect: ZIO[(Env, Input, StateIn), Err, (Output, StateOut)]
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
      self.effect.flatMap { case (output, stateOut) =>
        that.effect.provide((env, output, stateOut))
      }
    })

  /**
   * Maps the success value of this flow to the specified constant value.
   */
  def as[Out2](out: => Out2): Flow[StateIn, StateOut, Env, Input, Err, Out2] =
    self.map(_ => out)

  def flatMap[S, Env1 <: Env, In1 <: Input, Err1 >: Err, Out1](
    func: Output => Flow[StateOut, S, Env1, In1, Err1, Out1]
  ): Flow[StateIn, S, Env1, In1, Err1, Out1] =
    Flow(ZIO.environment[(Env1, In1, StateIn)].flatMap { case (env1, in1, _) =>
      self.effect.flatMap { case (out, stateOut) =>
        func(out).effect.provide((env1, in1, stateOut))
      }
    })

  def map[Out2](fn: Output => Out2): Flow[StateIn, StateOut, Env, Input, Err, Out2]                                   =
    Flow(self.effect.map { case (out, state) => (fn(out), state) })

  def mapEffect[Out2](fn: Output => Out2)(implicit ev: Err <:< Throwable): RFlow[StateIn, StateOut, Env, Input, Out2] =
    Flow(self.effect.mapEffect { case (out, state) => (fn(out), state) })

  def mapState[SOut2](fn: StateOut => SOut2): Flow[StateIn, SOut2, Env, Input, Err, Output]                           =
    Flow(self.effect.map { case (out, state) => (out, fn(state)) })

  def run(implicit
    evAnyInput: Any <:< Input,
    evAnyState: Any <:< StateIn
  ): ZIO[Env, Err, FlowSuccess[Output, StateOut]]                                                                     =
    self.effect.provideSome[Env](env => (env, (), ())).map(FlowSuccess.fromTuple)

  /**
   * Maps the output state value of this flow to the specified constant value.
   */
  def stateAs[StateOut2](stateOut: => StateOut2): Flow[StateIn, StateOut2, Env, Input, Err, Output] =
    self.mapState(_ => stateOut)
}

object Flow {
  def fromFunction[In, Out](func: In => Out): Step[Any, In, Nothing, Out] =
    Flow(ZIO.environment[(Any, In, Any)].map { case (_, in, _) =>
      (func(in), ())
    })

  /**
   * Returns a flow with the empty value.
   */
  val none: UStep[Any, Option[Nothing]] =
    Flow(ZIO.environment[(Any, Any, Any)].as((None, ())))

  /**
   * A flow that succeeds with a unit value.
   */
  val unit: UStep[Any, Unit] =
    Flow(ZIO.environment[(Any, Any, Any)].as(((), ())))

  def succeed[A](value: => A): Flow[Any, Unit, Any, Any, Nothing, A] =
    Flow(ZIO.environment[(Any, Any, Any)].as((value, ())))

  def fail[Err](error: Err): Flow[Any, Nothing, Any, Any, Err, Nothing] =
    Flow(ZIO.environment[(Any, Any, Any)] *> ZIO.fail(error))
}
