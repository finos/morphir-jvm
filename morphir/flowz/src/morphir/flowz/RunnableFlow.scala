//package morphir.flowz
//
//import zio._
//
//trait RunnableFlow[-RIn, +E, +ROut, +Options, +Cfg, +Inputs] {
//
//  def getOptions(args: List[String], variables: Map[String, String]): ZIO[RIn, E, Options]
//
//  def getConfiguration[Opts >: Options](options: Opts): ZIO[RIn, E, Cfg]
//
//  def configureEnvironment[Opts >: Options, Configuration >: Cfg](
//    options: Opts,
//    config: Configuration
//  ): URIO[RIn, ZLayer[RIn, E, ROut]]
//
//  def prepareInputs[R >: ROut, Opts >: Options, Configuration >: Cfg](
//    options: Opts,
//    config: Configuration
//  ): ZIO[R, E, Inputs]
//
//  def onInitializationFailure[E1 >: E](failure: E1): ZIO[RIn, Nothing, ExitCode]
//  def onStepFailure[R >: ROut, E1 >: E](failure: E1): ZIO[R, Nothing, ExitCode]
//  def onStepSuccess[R >: ROut, A](success: A): ZIO[R, Nothing, ExitCode]
//
//  def run[R >: ROut, StepInputs >: Inputs, E1 >: E, Out](
//    step: Step[R, StepInputs, E1, Out]
//  )(args: CommandLineArgs, variables: Variables): ZIO[RIn, Nothing, ExitCode] = {
//
//    def runStep(options: Options, configuration: Cfg, layer: ZLayer[RIn, E, ROut]) =
//      (for {
//        inputs <- prepareInputs(options, configuration)
//        output <- step.run(inputs)
//      } yield output).provideLayer(layer).foldM(onStepFailure, onStepSuccess)
//
//    val program = for {
//      options       <- getOptions(args, variables)
//      configuration <- getConfiguration(options)
//      stepLayer     <- configureEnvironment(options = options, config = configuration)
//      exitCode      <- runStep(options, configuration, stepLayer)
//    } yield exitCode
//
//    program.foldM(
//      onInitializationFailure(_),
//      exitCode => ZIO.succeed(exitCode)
//    )
//  }
//}
//
//object RunnableFlow {
//
//  sealed trait FlowExecFailure[+Err] extends Exception with Product with Serializable {}
//
//  object FlowExecFailure {
//    final case class InitializationError[+Err](error: Err)   extends FlowExecFailure[Err]
//    final case class ConfigurationError[+Err](error: Err)    extends FlowExecFailure[Err]
//    final case class InputPreparationError[+Err](error: Err) extends FlowExecFailure[Err]
//    final case class StepExecutionError[+Err](error: Err)    extends FlowExecFailure[Err]
//    //final case class OptionsInitializationError[+Err](error: Err) extends FlowExecFailure[Err]
//  }
//
//  sealed trait ExecutionPhase
//  object ExecutionPhase {
//    case object GetOptions
//  }
//
//}
//
//final case class FlowContext[+Params]()
