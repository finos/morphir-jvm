package morphir
import zio.prelude._

package object flowz {

  type RFlow[-StateIn, +StateOut, -Env, -In, +Out] = Flow[StateIn, StateOut, Env, In, Throwable, Out]
  type Step[-Env, -In, +Err, +Out]                 = Flow[Any, Unit, Env, In, Err, Out]

  type RStep[-In, +Out] = Flow[Any, Unit, Any, In, Throwable, Out]

  type TaskStep[-In, +Out] = Flow[Any, Unit, Any, In, Throwable, Out]

  type UStep[-In, +Out] = Flow[Any, Unit, Any, In, Nothing, Out]

  type FlowOutput[+Output] = FlowSuccess[Output, Unit]

  object CommandLineArgs extends Subtype[List[String]]
  type CommandLineArgs = CommandLineArgs.Type

  object Variables extends Subtype[Map[String, String]]
  type Variables = Variables.Type

  type FlowHostContext[+R] = (R, CommandLineArgs, Variables)

  type UFlowHost[+HostParams] = FlowHost[Any, Nothing, HostParams]
}
