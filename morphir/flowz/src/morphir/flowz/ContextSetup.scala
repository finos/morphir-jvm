package morphir.flowz

import zio._

/**
 * ContextSetup provides a recipe for building the context which a flow needs to execute.
 */
trait ContextSetup[-StartupEnv, -Input, +Env, +State, +Params] { self =>

  def &&[StartupEnv1 <: StartupEnv, Input1 <: Input, Env1, State1, Params1](
    other: ContextSetup[StartupEnv1, Input1, Env1, State1, Params1]
  )(implicit
    ev: Has.Union[Env, Env1]
  ): ContextSetup[StartupEnv1, Input1, Env with Env1, (State, State1), (Params, Params1)] =
    ContextSetup[StartupEnv1, Input1, Env with Env1, (State, State1), (Params, Params1)] { input: Input1 =>
      self.recipe.provideSome[StartupEnv1]((_, input)).zipWith(other.recipe.provideSome[StartupEnv1]((_, input))) {
        case (
              StepContext(leftEnv, StepInputs(leftState, leftParams)),
              StepContext(rightEnv, StepInputs(rightState, rightParams))
            ) =>
          StepContext(
            environment = ev.union(leftEnv, rightEnv),
            state = (leftState, rightState),
            params = (leftParams, rightParams)
          )
      }
    }

  def makeContext(input: Input): RIO[StartupEnv, StepContext[Env, State, Params]] =
    recipe.provideSome[StartupEnv](env => (env, input))

  /**
   * The effect that can be used to build the `StepContext`
   */
  def recipe: ZIO[(StartupEnv, Input), Throwable, StepContext[Env, State, Params]]

  /**
   * Apply further configuration.
   */
  def configure[StartupEnv1, Input1, Env1, State1, Params1](
    func: ContextSetup[StartupEnv, Input, Env, State, Params] => ContextSetup[
      StartupEnv1,
      Input1,
      Env1,
      State1,
      Params1
    ]
  ): ContextSetup[StartupEnv1, Input1, Env1, State1, Params1] = func(self)

  /**
   * Parse the parameters of the input to this flow from a command line
   */
  def parseCommandLineWith[CmdLine <: Input, Params1 >: Params](parse: CmdLine => Params1)(implicit
    ev: CmdLine <:< List[String]
  ): ContextSetup[StartupEnv, CmdLine, Env, State, Params1] =
    ContextSetup[StartupEnv, CmdLine, Env, State, Params1](cmdLine =>
      self.recipe
        .flatMap(context => ZIO.effect(context.updateParams(parse(cmdLine))))
        .provideSome[StartupEnv](env => (env, cmdLine))
    )
}

object ContextSetup {

  def apply[StartupEnv, Input, Env, State, Params](
    f: Input => RIO[StartupEnv, StepContext[Env, State, Params]]
  ): ContextSetup[StartupEnv, Input, Env, State, Params] =
    new ContextSetup[StartupEnv, Input, Env, State, Params] {
      def recipe: ZIO[(StartupEnv, Input), Throwable, StepContext[Env, State, Params]] =
        ZIO.accessM[(StartupEnv, Input)] { case (env, input) =>
          f(input).provide(env)
        }
    }

  /**
   * Create a `ContextSetup` from a setup function (which is potentially side-effecting).
   */
  def create[Input, Env, State, Params](
    setupFunc: Input => StepContext[Env, State, Params]
  ): ContextSetup[Any, Input, Env, State, Params] =
    ContextSetup { input: Input => ZIO.effect(setupFunc(input)) }

  /**
   * Create an instance of the default StepContextConfig.
   * This configuration only requires ZIO's ZEnv, and accepts command line args (as a list of strings).
   */
  val default: ContextSetup[ZEnv, List[String], ZEnv, Any, Any] = ContextSetup { _: List[String] =>
    ZIO.access[ZEnv](StepContext.fromEnvironment)
  }

  def deriveParams[Input, Params](
    f: Input => Params
  ): ContextSetup[Any, Input, Any, Any, Params] =
    ContextSetup { input =>
      ZIO
        .accessM[(Any, Input)] { case (env, input) =>
          ZIO.effect(StepContext(environment = env, state = (), params = f(input)))
        }
        .provide(((), input))
    }

  val empty: ContextSetup[Any, Any, Any, Any, Any] = ContextSetup { _: Any =>
    ZIO.succeed(StepContext.any)
  }

  def requiresStartupEnvironmentOfType[R]: ContextSetup[R, Any, Any, Any, Any] =
    new ContextSetup[R, Any, Any, Any, Any] {

      /**
       * The effect that can be used to build the `StepContext`
       */
      def recipe: ZIO[(R, Any), Throwable, StepContext[Any, Any, Any]] = ZIO.access[(R, Any)](_ => StepContext.any)
    }

  /**
   * Creates a new context setup that requires input of the provided type
   */
  def requiresInputOfType[Input]: ContextSetup[Any, Input, Any, Any, Any] = ContextSetup { _: Input =>
    ZIO.succeed(StepContext(environment = (), state = (), params = ()))
  }

  /**
   * Create a context setup from an effectual function that parses a command line.
   */
  def forCommandLineApp[StartupEnv <: Has[_], Params](
    func: List[String] => RIO[StartupEnv, Params]
  ): ContextSetup[StartupEnv, List[String], StartupEnv, Any, Params] =
    ContextSetup[StartupEnv, List[String], StartupEnv, Any, Params]((cmdLineArgs: List[String]) =>
      ZIO.accessM[StartupEnv] { env =>
        func(cmdLineArgs).map(params => StepContext(environment = env, state = (), params = params))
      }
    )

  /**
   * Creates a context setup that uses its initial startup requirements as the environment of the created context.
   */
  def givenEnvironmentAtStartup[R]: ContextSetup[R, Any, R, Any, Any] = ContextSetup { _: Any =>
    ZIO.access[R](env => StepContext(environment = env, state = (), params = ()))
  }

  //def make[StartupEnv, Input, Env, State, Params](effect:ZIO[Input, Throwable, StepContext[Env, State, Params]])
  //def make[R, StartupEnv, Input, Env, State, Params](effect:ZIO[R, Throwable, StepContext[Env, State, Params]]) = ???

  val withNoRequirements: ContextSetup[Any, Any, Any, Any, Any] = ContextSetup { _: Any =>
    ZIO.succeed(StepContext(environment = (), state = (), params = ()))
  }
}
