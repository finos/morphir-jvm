package morphir

import zio.Fiber
import zio.prelude._

package object flowz {
  object api extends Api

  object CommandLineArgs extends Subtype[List[String]]
  type CommandLineArgs = CommandLineArgs.Type

  object Variables extends Subtype[Map[String, String]]
  type Variables = Variables.Type

  type FlowHostContext[+R] = (R, CommandLineArgs, Variables)

  type UFlowHost[+HostParams] = FlowHost[Any, Nothing, HostParams]

  type ForkedStep[-StateIn, +StateOut, -Env, -Params, +Err, +Output] =
    Step[StateIn, Unit, Env, Params, Nothing, Fiber.Runtime[Err, StepOutputs[StateOut, Output]]]
}
