package morphir

import zio.Fiber
import zio.prelude._

package object flowz {
  object api extends Api

  type Activity[-Env, -Params, +Err, +Value] = Act[Any, Value, Env, Params, Err, Value]
  type IOStep[-Params, +Err, +Value]         = Act[Any, Unit, Any, Params, Err, Value]
  type TaskStep[-Params, +Value]             = Act[Any, Unit, Any, Params, Throwable, Value]
  type UStep[-Params, +Value]                = Act[Any, Any, Any, Params, Nothing, Value]

  object CommandLineArgs extends Subtype[List[String]]
  type CommandLineArgs = CommandLineArgs.Type

  object Variables extends Subtype[Map[String, String]]
  type Variables = Variables.Type

  type FlowHostContext[+R] = (R, CommandLineArgs, Variables)

  type UFlowHost[+HostParams] = FlowHost[Any, Nothing, HostParams]

  type ForkedStep[-StateIn, +StateOut, -Env, -Params, +Err, +Output] =
    Act[StateIn, Unit, Env, Params, Nothing, Fiber.Runtime[Err, StepOutputs[StateOut, Output]]]
}
