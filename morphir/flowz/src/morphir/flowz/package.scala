package morphir

import zio.{ Fiber, ZManaged }
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

  type ReturnBehavior[+A]     = Behavior[Any, Any, Any, Any, Nothing, A]
  type EffectBehavior[+E, +A] = Behavior[Any, Any, Any, Any, E, A]

  def process[In, R, Err, Out](label: String)(children: Flow[In, R, Err, Out]*): Flow[In, R, Err, Out] =
    Flow.process(label, ZManaged.succeed(children.toVector))

  def step[SIn, SOut, In, R, Err, Out](label: String)(
    behavior: Behavior[SIn, SOut, In, R, Err, Out]
  ): Flow[In, R, Err, Out] = Flow.step(label, behavior)
}
