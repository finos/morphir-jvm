package morphir
import zio.prelude._

package object flowz {

  type SrcFlow[+StateOut, +Out]                     = Flow[Any, StateOut, Any, Any, Nothing, Out]
  type Stage[-StateIn, +StateOut, -In, +Out]        = Flow[StateIn, StateOut, Any, In, Nothing, Out]
  type URFlow[-StateIn, +StateOut, -Env, -In, +Out] = Flow[StateIn, StateOut, Env, In, Nothing, Out]
  type RFlow[-StateIn, +StateOut, -Env, -In, +Out]  = Flow[StateIn, StateOut, Env, In, Throwable, Out]
  type TaskFlow[-StateIn, +StateOut, -In, +Out]     = Flow[StateIn, StateOut, Any, In, Throwable, Out]

  type Step[-Env, -In, +Err, +Out] = Flow[Any, Unit, Env, In, Err, Out]

  type RStep[-Env, -In, +Out] = Flow[Any, Unit, Env, In, Throwable, Out]

  type TaskStep[-In, +Out] = Flow[Any, Unit, Any, In, Throwable, Out]

  type UStep[-In, +Out] = Flow[Any, Unit, Any, In, Nothing, Out]

  type URStep[-Env, -In, +Out] = Flow[Any, Unit, Env, In, Nothing, Out]

  type FlowValue[+Output] = OutputChannels[Unit, Output]
  type FlowState[+S]      = OutputChannels[S, Unit]

  object CommandLineArgs extends Subtype[List[String]]
  type CommandLineArgs = CommandLineArgs.Type

  object Variables extends Subtype[Map[String, String]]
  type Variables = Variables.Type

  type FlowHostContext[+R] = (R, CommandLineArgs, Variables)

  type UFlowHost[+HostParams] = FlowHost[Any, Nothing, HostParams]
}
