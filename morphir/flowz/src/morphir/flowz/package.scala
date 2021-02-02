package morphir

import zio._
import zio.prelude._

package object flowz {
  object api extends Api

  type Activity[-Env, -Params, +Err, +Value] = Act[Any, Value, Env, Params, Err, Value]
  type IOAct[-Params, +Err, +Value]          = Act[Any, Unit, Any, Params, Err, Value]
  type TaskAct[-Params, +Value]              = Act[Any, Unit, Any, Params, Throwable, Value]
  type UAct[-Params, +Value]                 = Act[Any, Any, Any, Params, Nothing, Value]

  object CommandLineArgs extends Subtype[List[String]]
  type CommandLineArgs = CommandLineArgs.Type

  object Variables extends Subtype[Map[String, String]]
  type Variables = Variables.Type

  type FlowHostContext[+R] = (R, CommandLineArgs, Variables)

  type UFlowHost[+HostParams] = FlowHost[Any, Nothing, HostParams]

  type ForkedStep[-StateIn, +StateOut, -Env, -Params, +Err, +Output] =
    Act[StateIn, Unit, Env, Params, Nothing, Fiber.Runtime[Err, StepOutputs[StateOut, Output]]]

  type BehaviorEffect[-SIn, +SOut, -Msg, -Env, +E, +A] = ZIO[(SIn, Msg, Env), E, BehaviorResult[SOut, A]]
  type ReturnBehavior[+A]                              = Behavior[Any, Any, Any, Any, Nothing, A]
  type EffectBehavior[+S, +E, +A]                      = Behavior[Any, S, Any, Any, E, A]

//  def behavior[InputState, OutputState, Msg, R, Err, A](
//    f: (InputState, Msg) => ZIO[R, Err, (OutputState, A)]
//  )(implicit ev: CanFail[Err]): Behavior[InputState, OutputState, Msg, R, Err, A] =
//    Behavior[InputState, OutputState, Msg, R, Err, A](f)

//  def behavior[SIn, OutputState, Msg, R, E, A](
//    effect: ZIO[R with InputState[SIn], E, A]
//  ): Behavior[SIn, OutputState, Msg, R, Nothing, A] =
//    Behavior[SIn, OutputState, Msg, R, E, A](effect)

  def behavior[InputState, OutputState, Msg, R, A](
    f: (InputState, Msg) => URIO[R, BehaviorResult[OutputState, A]]
  ): Behavior[InputState, OutputState, Msg, R, Nothing, A] =
    Behavior.behaviorFromFunctionM(f)

  def outputting[OutputState, Value](state: OutputState, value: Value): EffectBehavior[OutputState, Nothing, Value] =
    Behavior.outputting(state, value)

  def process[SIn, SOut, In, R, Err, Out](label: String)(
    children: Flow[SIn, SOut, In, R, Err, Out]*
  ): Flow[SIn, SOut, In, R, Err, Out] =
    Flow.process(label, ZManaged.succeed(children.toVector))

  def step[SIn, SOut, In, R, Err, Out](label: String)(
    behavior: Behavior[SIn, SOut, In, R, Err, Out]
  ): Flow[SIn, SOut, In, R, Err, Out] = Flow.step(label, behavior)

}
