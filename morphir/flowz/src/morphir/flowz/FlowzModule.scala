package morphir.flowz

import zio._
import zio.clock.Clock

import scala.util.Try

trait FlowzModule extends Types with Channels with Context {

  type USrcFlow[+StateOut, +Out]                    = Flow[Any, StateOut, Any, Any, Nothing, Out]
  type SrcFlow[+StateOut, +Err, +Out]               = Flow[Any, StateOut, Any, Any, Err, Out]
  type Stage[-StateIn, +StateOut, -In, +Out]        = Flow[StateIn, StateOut, Any, In, Nothing, Out]
  type URFlow[-StateIn, +StateOut, -Env, -In, +Out] = Flow[StateIn, StateOut, Env, In, Nothing, Out]
  type RFlow[-StateIn, +StateOut, -Env, -In, +Out]  = Flow[StateIn, StateOut, Env, In, Throwable, Out]
  type TaskFlow[-StateIn, +StateOut, -In, +Out]     = Flow[StateIn, StateOut, Any, In, Throwable, Out]
  type ForkedFlow[-StateIn, +StateOut, -Env, -Params, +Err, +Output] =
    Flow[StateIn, Unit, Env, Params, Nothing, Fiber.Runtime[Err, OutputChannels[StateOut, Output]]]
  type Step[-Env, -In, +Err, +Out] = Flow[Any, Out, Env, In, Err, Out]

  type RStep[-Env, -In, +Out] = Flow[Any, Out, Env, In, Throwable, Out]

  type TaskStep[-In, +Out] = Flow[Any, Out, Any, In, Throwable, Out]

  type UStep[-In, +Out] = Flow[Any, Out, Any, In, Nothing, Out]

  type URStep[-Env, -In, +Out] = Flow[Any, Out, Env, In, Nothing, Out]

  /**
   * A flow describes an operation which may have some state and input operations.
   */
  sealed case class Flow[-StateIn, +StateOut, -Env, -Params, +Err, +Output](
    private[flowz] val effect: ZIO[FlowContext[Env, StateIn, Params], Err, OutputChannels[StateOut, Output]]
  ) { self =>

    /**
     * Connect this flow to the given flow. By connecting the output state of this flow to the input state of the other flow
     * and by connecting the output value of this flow to the input of the other.
     */
    def >>>[SOut2, Env1 <: Env, Err1 >: Err, Output2](
      that: Flow[StateOut, SOut2, Env1, Output, Err1, Output2]
    ): Flow[StateIn, SOut2, Env1, Params, Err1, Output2] =
      self andThen that

    def *>[StateIn1 <: StateIn, Env1 <: Env, Params1 <: Params, Err1 >: Err, StateOut2, Output2](
      that: Flow[StateIn1, StateOut2, Env1, Params1, Err1, Output2]
    ): Flow[StateIn1, StateOut2, Env1, Params1, Err1, Output2] =
      Flow(self.effect *> that.effect)

    def <*>[StateIn1 <: StateIn, Env1 <: Env, In1 <: Params, Err1 >: Err, StateOut2, Output2](
      that: Flow[StateIn1, StateOut2, Env1, In1, Err1, Output2]
    ): Flow[StateIn1, (StateOut, StateOut2), Env1, In1, Err1, (Output, Output2)] = self zip that

    def |+|[StateIn1 <: StateIn, Env1 <: Env, In1 <: Params, Err1 >: Err, StateOut2, Output2](
      that: Flow[StateIn1, StateOut2, Env1, In1, Err1, Output2]
    ): Flow[StateIn1, (StateOut, StateOut2), Env1, In1, Err1, (Output, Output2)] =
      Flow((self.effect zipPar that.effect).map { case (left, right) => left zip right })

    def <&>[StateIn1 <: StateIn, Env1 <: Env, In1 <: Params, Err1 >: Err, StateOut2, Output2](
      that: Flow[StateIn1, StateOut2, Env1, In1, Err1, Output2]
    ): Flow[StateIn1, (StateOut, StateOut2), Env1, In1, Err1, (Output, Output2)] =
      Flow((self.effect zipPar that.effect).map { case (left, right) => left zip right })

    def <*[StateIn1 <: StateIn, Env1 <: Env, Params1 <: Params, Err1 >: Err, StateOut2, Output2](
      that: Flow[StateIn1, StateOut2, Env1, Params1, Err1, Output2]
    ): Flow[StateIn1, StateOut, Env1, Params1, Err1, Output] =
      Flow(self.effect <* that.effect)

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
      Flow(self.effect.map(out => out.value) andThen thatEffect)

    /**
     * Maps the success value of this flow to the specified constant value.
     */
    def as[Out2](out: => Out2): Flow[StateIn, StateOut, Env, Params, Err, Out2] = self.map(_ => out)

    def delay(duration: zio.duration.Duration): Flow[StateIn, StateOut, Env with Clock, Params, Err, Output] =
      Flow(
        for {
          ctx    <- ZIO.environment[FlowContext[Env with Clock, StateIn, Params]]
          result <- self.effect.provide(ctx).delay(duration).provide(ctx.environment)
        } yield result
      )

    def flatMap[S, Env1 <: Env, In1 <: Params, Err1 >: Err, Out1](
      func: Output => Flow[StateOut, S, Env1, In1, Err1, Out1]
    ): Flow[StateIn, S, Env1, In1, Err1, Out1] =
      Flow(ZIO.environment[FlowContext[Env1, StateIn, In1]].flatMap { ctx =>
        self.effect.flatMap(out => func(out.value).effect.provide(ctx.updateState(out.state)))
      })

    def flipOutputs: Flow[StateIn, Output, Env, Params, Err, StateOut] =
      self.mapOutputs { case (state, value) => (value, state) }

    def fork: ForkedFlow[StateIn, StateOut, Env, Params, Err, Output] =
      new Flow[StateIn, Unit, Env, Params, Nothing, Fiber.Runtime[Err, OutputChannels[StateOut, Output]]](
        self.effect.fork.map { rt =>
          OutputChannels(rt)
        }
      )

    def map[Out2](fn: Output => Out2): Flow[StateIn, StateOut, Env, Params, Err, Out2] = Flow(
      self.effect.map(success => success.map(fn))
    )

    def mapEffect[Out2](fn: Output => Out2)(implicit
      ev: Err <:< Throwable
    ): RFlow[StateIn, StateOut, Env, Params, Out2] =
      Flow(self.effect.mapEffect(success => success.map(fn)))

    def mapError[Err2](onError: Err => Err2): Flow[StateIn, StateOut, Env, Params, Err2, Output] =
      Flow(self.effect.mapError(onError))

    def mapOutputs[StateOut2, Output2](
      func: (StateOut, Output) => (StateOut2, Output2)
    ): Flow[StateIn, StateOut2, Env, Params, Err, Output2] =
      Flow(self.effect.map(out => OutputChannels.fromTuple(func(out.state, out.value))))

    def mapOutputChannels[StateOut2, Output2](
      func: FOuts[StateOut, Output] => FOuts[StateOut2, Output2]
    ): Flow[StateIn, StateOut2, Env, Params, Err, Output2] =
      Flow(self.effect.map(func))

    def mapState[SOut2](fn: StateOut => SOut2): Flow[StateIn, SOut2, Env, Params, Err, Output] = Flow(
      self.effect.map(success => success.mapState(fn))
    )

    def shiftStateToOutput: Flow[StateIn, Unit, Env, Params, Err, (StateOut, Output)] =
      Flow(effect.map(success => OutputChannels(state = (), value = (success.state, success.value))))

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
     * Make the state and the output value the same by making the state equal to the output.
     */
    def unifyOutputs: Flow[StateIn, Output, Env, Params, Err, Output] =
      self.mapOutputs { case (_, out) =>
        (out, out)
      }

    /**
     * Maps the output state value of this flow to the specified constant value.
     */
    def stateAs[StateOut2](stateOut: => StateOut2): Flow[StateIn, StateOut2, Env, Params, Err, Output] =
      self.mapState(_ => stateOut)

    def tap[Env1 <: Env, Err1 >: Err](
      func: (StateOut, Output) => ZIO[Env1, Err1, Any]
    ): Flow[StateIn, StateOut, Env1, Params, Err1, Output] =
      Flow(
        ZIO
          .environment[FlowContext[Env1, StateIn, Params]]
          .flatMap(ctx => self.effect.tap(out => func(out.state, out.value).provide(ctx.environment)))
      )

    def tapState[Env1 <: Env, Err1 >: Err](
      func: StateOut => ZIO[Env1, Err1, Any]
    ): Flow[StateIn, StateOut, Env1, Params, Err1, Output] =
      Flow(
        ZIO
          .environment[FlowContext[Env1, StateIn, Params]]
          .flatMap(ctx => self.effect.tap(out => func(out.state).provide(ctx.environment)))
      )

    def tapValue[Env1 <: Env, Err1 >: Err](
      func: Output => ZIO[Env1, Err1, Any]
    ): Flow[StateIn, StateOut, Env1, Params, Err1, Output] =
      Flow(
        ZIO
          .environment[FlowContext[Env1, StateIn, Params]]
          .flatMap(ctx => self.effect.tap(out => func(out.value).provide(ctx.environment)))
      )

    def transformEff[StateOut2, Output2](
      func: (StateOut, Output) => (StateOut2, Output2)
    )(implicit ev: Err <:< Throwable): Flow[StateIn, StateOut2, Env, Params, Throwable, Output2] =
      Flow(self.effect.mapEffect(out => OutputChannels.fromTuple(func(out.state, out.value))))

    def zip[StateIn1 <: StateIn, Env1 <: Env, In1 <: Params, Err1 >: Err, StateOut2, Output2](
      that: Flow[StateIn1, StateOut2, Env1, In1, Err1, Output2]
    ): Flow[StateIn1, (StateOut, StateOut2), Env1, In1, Err1, (Output, Output2)] =
      Flow((self.effect zip that.effect).map { case (left, right) => left zip right })

    def zipPar[StateIn1 <: StateIn, Env1 <: Env, In1 <: Params, Err1 >: Err, StateOut2, Output2](
      that: Flow[StateIn1, StateOut2, Env1, In1, Err1, Output2]
    ): Flow[StateIn1, (StateOut, StateOut2), Env1, In1, Err1, (Output, Output2)] =
      Flow((self.effect zipPar that.effect).map { case (left, right) => left zip right })

    def zipWith[
      StateIn1 <: StateIn,
      Env1 <: Env,
      In1 <: Params,
      Err1 >: Err,
      StateOut2,
      Output2,
      FinalState,
      FinalOutput
    ](that: Flow[StateIn1, StateOut2, Env1, In1, Err1, Output2])(
      f: (
        OutputChannels[StateOut, Output],
        OutputChannels[StateOut2, Output2]
      ) => OutputChannels[FinalState, FinalOutput]
    ): Flow[StateIn1, FinalState, Env1, In1, Err1, FinalOutput] =
      Flow((self.effect zipWith that.effect)(f))

    def zipWithPar[
      StateIn1 <: StateIn,
      Env1 <: Env,
      In1 <: Params,
      Err1 >: Err,
      StateOut2,
      Output2,
      FinalState,
      FinalOutput
    ](that: Flow[StateIn1, StateOut2, Env1, In1, Err1, Output2])(
      f: (FOuts[StateOut, Output], FOuts[StateOut2, Output2]) => FOuts[FinalState, FinalOutput]
    ): Flow[StateIn1, FinalState, Env1, In1, Err1, FinalOutput] =
      Flow((self.effect zipWithPar that.effect)(f))
  }

  object Flow extends FlowCompanion with AnyEnvFlowCompanion {

    def apply[StateIn, Params, StateOut, Out](
      func: (StateIn, Params) => (StateOut, Out)
    ): TaskFlow[StateIn, StateOut, Params, Out] =
      Flow(ZIO.environment[FlowContext.having.AnyEnv[StateIn, Params]].mapEffect { ctx =>
        val (state, value) = func(ctx.inputs.state, ctx.inputs.params)
        OutputChannels(state = state, value = value)
      })

    def environment[Env]: RStep[Env, Any, Env] =
      Flow(ZIO.environment[FlowContext.having.Environment[Env]].map(ctx => FlowValue.fromValue(ctx.environment)))

    def makeStep[Env, Params, Err, Out](func: Params => ZIO[Env, Err, Out]): Step[Env, Params, Err, Out] =
      Flow.parameters[Params].flatMap { params =>
        Flow.fromEffect(func(params))
      }
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
      Flow(ZIO.environment[FlowContext[Env, StateIn, Params]].map(OutputChannels(_)))

    def effect[Value](value: => Value): TaskStep[Any, Value] =
      Flow(ZIO.environment[FlowContext.having.AnyInputs].mapEffect(_ => OutputChannels.fromValue(value)))

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
        val value = func(ctx.inputs.params)
        OutputChannels(value = value, state = value)
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

    def fromOutputs[State, Output](channels: OutputChannels[State, Output]): USrcFlow[State, Output] =
      Flow(ZIO.succeed(channels))

    def fromTry[Value](value: => Try[Value]): TaskStep[Any, Value] =
      Flow(for {
        _     <- ZIO.environment[FlowContext.having.AnyInputs]
        value <- ZIO.fromTry(value)
      } yield OutputChannels.fromValue(value))

    def importOutputs[State, Err, Output](effect: IO[Err, OutputChannels[State, Output]]): SrcFlow[State, Err, Output] =
      Flow(effect)

    def join[State, Err, Output](fiber: Fiber[Err, OutputChannels[State, Output]]): SrcFlow[State, Err, Output] =
      Flow(fiber.join)

    def get[State]: Flow[State, State, Any, Any, Nothing, State] =
      modify[State, State, State](s => (s, s))

    def mapN[S0, R, P, Err, SA, A, SB, B, SC, C](flowA: Flow[S0, SA, R, P, Err, A], flowB: Flow[S0, SB, R, P, Err, B])(
      f: (FOuts[SA, A], FOuts[SB, B]) => FOuts[SC, C]
    ): Flow[S0, SC, R, P, Err, C] =
      flowA.zipWith(flowB)(f)

    def mapParN[S0, R, P, Err, SA, A, SB, B, SC, C](
      flowA: Flow[S0, SA, R, P, Err, A],
      flowB: Flow[S0, SB, R, P, Err, B]
    )(
      f: (FOuts[SA, A], FOuts[SB, B]) => FOuts[SC, C]
    ): Flow[S0, SC, R, P, Err, C] =
      flowA.zipWithPar(flowB)(f)

    def mapParN[S0, R, P, Err, SA, A, SB, B, SC, C, SD, D](
      flowA: Flow[S0, SA, R, P, Err, A],
      flowB: Flow[S0, SB, R, P, Err, B],
      flowC: Flow[S0, SC, R, P, Err, C]
    )(
      f: (FOuts[SA, A], FOuts[SB, B], FOuts[SC, C]) => FOuts[SD, D]
    ): Flow[S0, SD, R, P, Err, D] =
      (flowA <&> flowB <&> flowC).mapOutputChannels { case OutputChannels(((a, b), c), ((sa, sb), sc)) =>
        val outsA = OutputChannels(state = sa, value = a)
        val outsB = OutputChannels(state = sb, value = b)
        val outsC = OutputChannels(state = sc, value = c)
        f(outsA, outsB, outsC)
      }

    def mapParN[S0, R, P, Err, SA, A, SB, B, SC, C, SD, D, SF, F](
      flowA: Flow[S0, SA, R, P, Err, A],
      flowB: Flow[S0, SB, R, P, Err, B],
      flowC: Flow[S0, SC, R, P, Err, C],
      flowD: Flow[S0, SD, R, P, Err, D]
    )(
      f: (FOuts[SA, A], FOuts[SB, B], FOuts[SC, C], FOuts[SD, D]) => FOuts[SF, F]
    ): Flow[S0, SF, R, P, Err, F] =
      (flowA <&> flowB <&> flowC <&> flowD).mapOutputChannels {
        case OutputChannels((((a, b), c), d), (((sa, sb), sc), sd)) =>
          val outsA = OutputChannels(state = sa, value = a)
          val outsB = OutputChannels(state = sb, value = b)
          val outsC = OutputChannels(state = sc, value = c)
          val outsD = OutputChannels(state = sd, value = d)
          f(outsA, outsB, outsC, outsD)
      }

    def mapParN[S0, R, P, Err, S1, A1, S2, A2, S3, A3, S4, A4, S5, A5, S6, A6](
      flow1: Flow[S0, S1, R, P, Err, A1],
      flow2: Flow[S0, S2, R, P, Err, A2],
      flow3: Flow[S0, S3, R, P, Err, A3],
      flow4: Flow[S0, S4, R, P, Err, A4],
      flow5: Flow[S0, S5, R, P, Err, A5]
    )(
      f: (FOuts[S1, A1], FOuts[S2, A2], FOuts[S3, A3], FOuts[S4, A4], FOuts[S5, A5]) => FOuts[S6, A6]
    ): Flow[S0, S6, R, P, Err, A6] =
      (flow1 <&> flow2 <&> flow3 <&> flow4 <&> flow5).mapOutputChannels {
        case OutputChannels(((((a, b), c), d), e), ((((sa, sb), sc), sd), se)) =>
          val outsA = OutputChannels(state = sa, value = a)
          val outsB = OutputChannels(state = sb, value = b)
          val outsC = OutputChannels(state = sc, value = c)
          val outsD = OutputChannels(state = sd, value = d)
          val outsE = OutputChannels(state = se, value = e)
          f(outsA, outsB, outsC, outsD, outsE)
      }

    def mapParN[S0, R, P, Err, S1, A1, S2, A2, S3, A3, S4, A4, S5, A5, S6, A6, S7, A7](
      flow1: Flow[S0, S1, R, P, Err, A1],
      flow2: Flow[S0, S2, R, P, Err, A2],
      flow3: Flow[S0, S3, R, P, Err, A3],
      flow4: Flow[S0, S4, R, P, Err, A4],
      flow5: Flow[S0, S5, R, P, Err, A5],
      flow6: Flow[S0, S6, R, P, Err, A6]
    )(
      func: (FOuts[S1, A1], FOuts[S2, A2], FOuts[S3, A3], FOuts[S4, A4], FOuts[S5, A5], FOuts[S6, A6]) => FOuts[S7, A7]
    ): Flow[S0, S7, R, P, Err, A7] =
      (flow1 <&> flow2 <&> flow3 <&> flow4 <&> flow5 <&> flow6).mapOutputChannels {
        case OutputChannels((((((a, b), c), d), e), f), (((((sa, sb), sc), sd), se), sf)) =>
          val outsA = OutputChannels(state = sa, value = a)
          val outsB = OutputChannels(state = sb, value = b)
          val outsC = OutputChannels(state = sc, value = c)
          val outsD = OutputChannels(state = sd, value = d)
          val outsE = OutputChannels(state = se, value = e)
          val outsF = OutputChannels(state = sf, value = f)
          func(outsA, outsB, outsC, outsD, outsE, outsF)
      }

    def mapParN[S0, R, P, Err, S1, A1, S2, A2, S3, A3, S4, A4, S5, A5, S6, A6, S7, A7, S8, A8](
      flow1: Flow[S0, S1, R, P, Err, A1],
      flow2: Flow[S0, S2, R, P, Err, A2],
      flow3: Flow[S0, S3, R, P, Err, A3],
      flow4: Flow[S0, S4, R, P, Err, A4],
      flow5: Flow[S0, S5, R, P, Err, A5],
      flow6: Flow[S0, S6, R, P, Err, A6],
      flow7: Flow[S0, S7, R, P, Err, A7]
    )(
      func: (
        FOuts[S1, A1],
        FOuts[S2, A2],
        FOuts[S3, A3],
        FOuts[S4, A4],
        FOuts[S5, A5],
        FOuts[S6, A6],
        FOuts[S7, A7]
      ) => FOuts[S8, A8]
    ): Flow[S0, S8, R, P, Err, A8] =
      (flow1 <&> flow2 <&> flow3 <&> flow4 <&> flow5 <&> flow6 <&> flow7).mapOutputChannels {
        case OutputChannels(((((((a, b), c), d), e), f), g), ((((((sa, sb), sc), sd), se), sf), sg)) =>
          val outsA = OutputChannels(state = sa, value = a)
          val outsB = OutputChannels(state = sb, value = b)
          val outsC = OutputChannels(state = sc, value = c)
          val outsD = OutputChannels(state = sd, value = d)
          val outsE = OutputChannels(state = se, value = e)
          val outsF = OutputChannels(state = sf, value = f)
          val outsG = OutputChannels(state = sg, value = g)
          func(outsA, outsB, outsC, outsD, outsE, outsF, outsG)
      }

    def mapParN[S0, R, P, Err, S1, A1, S2, A2, S3, A3, S4, A4, S5, A5, S6, A6, S7, A7, S8, A8, S9, A9](
      flow1: Flow[S0, S1, R, P, Err, A1],
      flow2: Flow[S0, S2, R, P, Err, A2],
      flow3: Flow[S0, S3, R, P, Err, A3],
      flow4: Flow[S0, S4, R, P, Err, A4],
      flow5: Flow[S0, S5, R, P, Err, A5],
      flow6: Flow[S0, S6, R, P, Err, A6],
      flow7: Flow[S0, S7, R, P, Err, A7],
      flow8: Flow[S0, S8, R, P, Err, A8]
    )(
      func: (
        FOuts[S1, A1],
        FOuts[S2, A2],
        FOuts[S3, A3],
        FOuts[S4, A4],
        FOuts[S5, A5],
        FOuts[S6, A6],
        FOuts[S7, A7],
        FOuts[S8, A8]
      ) => FOuts[S9, A9]
    ): Flow[S0, S9, R, P, Err, A9] =
      (flow1 <&> flow2 <&> flow3 <&> flow4 <&> flow5 <&> flow6 <&> flow7 <&> flow8).mapOutputChannels {
        case OutputChannels((((((((a, b), c), d), e), f), g), h), (((((((sa, sb), sc), sd), se), sf), sg), sh)) =>
          val outsA = OutputChannels(state = sa, value = a)
          val outsB = OutputChannels(state = sb, value = b)
          val outsC = OutputChannels(state = sc, value = c)
          val outsD = OutputChannels(state = sd, value = d)
          val outsE = OutputChannels(state = se, value = e)
          val outsF = OutputChannels(state = sf, value = f)
          val outsG = OutputChannels(state = sg, value = g)
          val outsH = OutputChannels(state = sh, value = h)
          func(outsA, outsB, outsC, outsD, outsE, outsF, outsG, outsH)
      }

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
    ): Flow[StateIn, StateOut, Any, Params, Nothing, Out] =
      Flow(ZIO.environment[FlowContext.having.AnyEnv[StateIn, Params]].map { ctx =>
        val (state, value) = func(ctx.inputs.state, ctx.inputs.params)
        OutputChannels(state = state, value = value)
      })

    def statefulEffect[StateIn, Params, StateOut, Out](
      func: (StateIn, Params) => (StateOut, Out)
    ): Flow[StateIn, StateOut, Any, Params, Throwable, Out] =
      Flow(ZIO.environment[FlowContext.having.AnyEnv[StateIn, Params]].mapEffect { ctx =>
        val (state, value) = func(ctx.inputs.state, ctx.inputs.params)
        OutputChannels(state = state, value = value)
      })

    def succeed[A](value: => A): UStep[Any, A] =
      Flow(ZIO.environment[FlowContext.having.AnyInputs].as(OutputChannels.fromValue(value)))

    def succeed[Value, State](value: => Value, state: => State): USrcFlow[State, Value] =
      Flow(ZIO.environment[FlowContext.having.AnyInputs].as(OutputChannels(value = value, state = state)))

    def task[In, Out](func: In => Out): TaskStep[In, Out] =
      Flow(ZIO.environment[FlowContext.having.Parameters[In]].mapEffect { ctx =>
        val value = func(ctx.inputs.params)
        OutputChannels(value = value, state = value)
      })
  }

  object Step extends FlowCompanion with AnyEnvFlowCompanion {
    def environment[Env]: RStep[Env, Any, Env] =
      Flow(ZIO.environment[FlowContext.having.Environment[Env]].map(ctx => FlowValue.fromValue(ctx.environment)))

    def makeStep[Env, Params, Err, Out](func: Params => ZIO[Env, Err, Out]): Step[Env, Params, Err, Out] =
      Flow.parameters[Params].flatMap { params =>
        Flow.fromEffect(func(params))
      }
  }

  object RStep extends FlowCompanion {
    def makeStep[Env, Params, Err, Out](func: Params => ZIO[Env, Err, Out]): Step[Env, Params, Err, Out] =
      Flow.parameters[Params].flatMap { params =>
        Flow.fromEffect(func(params))
      }
  }

  object TaskStep extends FlowCompanion with AnyEnvFlowCompanion {
    def makeStep[Env, Params, Err, Out](func: Params => ZIO[Env, Err, Out]): Step[Env, Params, Err, Out] =
      Flow.parameters[Params].flatMap { params =>
        Flow.fromEffect(func(params))
      }
  }

  object UStep extends FlowCompanion with AnyEnvFlowCompanion {
    def makeStep[Env, Params, Err, Out](func: Params => ZIO[Env, Err, Out]): Step[Env, Params, Err, Out] =
      Flow.parameters[Params].flatMap { params =>
        Flow.fromEffect(func(params))
      }
  }

  private[flowz] trait AnyEnvFlowCompanion {

    /**
     * A step that returns the given parameters.
     */
    def parameters[P]: UStep[P, P] =
      Flow.context[Any, Any, P].flatMap { ctx =>
        Flow.succeed(ctx.inputs.params, ctx.inputs.params)
      }
  }

  trait FiberSyntax {
    import FiberSyntax._
    implicit def toFiberOutputChannelOps[State, Err, Output](
      fiber: Fiber[Err, OutputChannels[State, Err]]
    ): FiberOutputChannelOps[State, Err, Output] =
      new FiberOutputChannelOps[State, Err, Output](fiber)

  }

  object FiberSyntax {
    class FiberOutputChannelOps[+State, +Err, +Output](val self: Fiber[Err, OutputChannels[State, Err]]) extends {
      def joinFlow: SrcFlow[State, Err, Err] = Flow.join(self)
    }
  }

  abstract class AbstractFlow {
    type Params
    type CommandLineParsingError

    def parseCommandLine(args: List[String]): IO[CommandLineParsingError, Params]

    def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
      (for {
        params <- parseCommandLine((args))
      } yield params).foldM(
        error => console.putStrLn(s"Error occurred: $error") *> ZIO.succeed(ExitCode.failure),
        params => console.putStrLn(s"Parsed command line params to: $params") *> ZIO.succeed(ExitCode.success)
      )
  }

  abstract class DefaultRunnableFlow extends AbstractFlow with App {}
}
