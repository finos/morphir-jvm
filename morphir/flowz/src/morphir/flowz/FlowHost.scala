package morphir.flowz

import zio._

final case class FlowHost[-HostEnv, +Err, +HostParams](
  private val effect: ZIO[FlowHostContext[HostEnv], Err, HostParams]
) { self =>

  def map[HostParams2](fn: HostParams => HostParams2): FlowHost[HostEnv, Err, HostParams2] =
    FlowHost(self.effect.map(fn))

  def flatMap[R1 <: HostEnv, E1 >: Err, P](fn: HostParams => FlowHost[R1, E1, P]): FlowHost[R1, E1, P] =
    FlowHost(self.effect.flatMap(fn(_).effect))

  def provideCommandLine(commandLineArgs: CommandLineArgs): FlowHost[HostEnv, Err, HostParams] =
    FlowHost(effect.provideSome[(HostEnv, Any, Variables)] { case (env, _, variables) =>
      (env, commandLineArgs, variables)
    })

  def provideHostEnv(hostEnv: HostEnv): FlowHost[Any, Err, HostParams] =
    FlowHost(self.effect.provideSome[(Any, CommandLineArgs, Variables)] { case (_, args, variables) =>
      (hostEnv, args, variables)
    })

  def provideVariables(variables: Variables): FlowHost[HostEnv, Err, HostParams] =
    FlowHost(effect.provideSome[(HostEnv, CommandLineArgs, Any)] { case (env, args, _) =>
      (env, args, variables)
    })

  def getParams(args: CommandLineArgs, variables: Variables) =
    self.effect.provideSome[HostEnv](env => (env, args, variables))

  def withCommandLine(commandLineArgs: CommandLineArgs): FlowHost[HostEnv, Err, HostParams] =
    provideCommandLine(commandLineArgs)

  def withHostEnv(hostEnv: HostEnv): FlowHost[Any, Err, HostParams] =
    provideHostEnv(hostEnv)

  def withVariables(variables: Variables): FlowHost[HostEnv, Err, HostParams] =
    provideVariables(variables)
}

object FlowHost {

  def apply(
    args: CommandLineArgs
  ): UFlowHost[(CommandLineArgs, Variables)] =
    FlowHost(ZIO.succeed((args, Variables(sys.env))))

  def apply(
    args: CommandLineArgs,
    variables: Variables
  ): UFlowHost[(CommandLineArgs, Variables)] =
    FlowHost(
      ZIO.environment[FlowHostContext[Any]].provide(((), args, variables)).map { case (_, args, variables) =>
        (args, variables)
      }
    )

  def args: UFlowHost[CommandLineArgs] =
    FlowHost(ZIO.environment[FlowHostContext[Any]].map(_._2))

  def variables: UFlowHost[Variables] =
    FlowHost(ZIO.environment[FlowHostContext[Any]].map(_._3))
}
