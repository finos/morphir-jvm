package morphir.flowz

import zio._

trait RunnableFlow[-RIn, +E, +ROut, +Options, +Cfg, +Inputs] {

  def getOptions(args: List[String], variables: Map[String, String]): ZIO[RIn, E, Options]

  def getConfiguration[Opts >: Options](options: Opts): ZIO[RIn, E, Cfg]

  def configureEnvironment[Opts >: Options, Configuration >: Cfg](
    options: Opts,
    config: Configuration
  ): URIO[RIn, ZLayer[RIn, E, ROut]]

  def prepareInputs[R >: ROut, Opts >: Options, Configuration >: Cfg](
    options: Opts,
    config: Configuration
  ): ZIO[R, E, Inputs]

  def run[R >: ROut, StepInputs >: Inputs, E1 >: E, Out](
    step: Step[R, StepInputs, E1, Out]
  ): (CommandLineArgs, Variables) => ZIO[RIn, E1, Out] = { case (args, variables) =>
    for {
      options       <- getOptions(args, variables)
      configuration <- getConfiguration(options)
      stepLayer     <- configureEnvironment(options = options, config = configuration)
      inputs        <- prepareInputs(options, configuration).provideLayer(stepLayer)
      result        <- step.run(inputs).provideLayer(stepLayer)
    } yield result
  }
}

object RunnableFlow {}

final case class FlowContext[+Params]()
