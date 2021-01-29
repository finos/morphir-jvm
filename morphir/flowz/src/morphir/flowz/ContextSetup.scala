package morphir.flowz

import com.github.ghik.silencer.silent
import zio._

/**
 * ContextSetup provides a recipe for building the context which a flow needs to execute.
 */
trait ContextSetup[-StartupEnv, -Input, +Env, +State, +Params] { self =>

  def &&[StartupEnv1 <: StartupEnv, Input1 <: Input, Env0 >: Env, Env1, State1, Params1](
    other: ContextSetup[StartupEnv1, Input1, Env1, State1, Params1]
  )(implicit
    ev: Has.Union[Env0, Env1]
  ): ContextSetup[StartupEnv1, Input1, Env0 with Env1, (State, State1), (Params, Params1)] =
    ContextSetup[StartupEnv1, Input1, Env0 with Env1, (State, State1), (Params, Params1)] { input: Input1 =>
      self.recipe.provideSome[StartupEnv1]((_, input)).zipWith(other.recipe.provideSome[StartupEnv1]((_, input))) {
        case (
              StageContext(leftEnv, StepInputs(leftState, leftParams)),
              StageContext(rightEnv, StepInputs(rightState, rightParams))
            ) =>
          StageContext(
            environment = ev.union(leftEnv, rightEnv),
            state = (leftState, rightState),
            params = (leftParams, rightParams)
          )
      }
    }

  def andUses[StartupEnv1]: ContextSetup[StartupEnv with StartupEnv1, Input, Env, State, Params] = ContextSetup {
    input =>
      ZIO.accessM[StartupEnv with StartupEnv1](env => self.recipe.provide((env, input)))
  }

  def extractParamsWith[StartupEnv1 <: StartupEnv, Input1 <: Input, Params1](
    func: Input1 => RIO[StartupEnv1, Params1]
  ): ContextSetup[StartupEnv1, Input1, Env, State, Params1] = ContextSetup[StartupEnv1, Input1, Env, State, Params1] {
    input =>
      ZIO.accessM[StartupEnv1](env =>
        self.recipe.flatMap(ctx => func(input).map(ctx.updateParams).provide(env)).provide((env, input))
      )
  }

  def provideSomeInput[In](adapter: In => Input): ContextSetup[StartupEnv, In, Env, State, Params] =
    ContextSetup[StartupEnv, In, Env, State, Params](input =>
      self.recipe.provideSome[StartupEnv](env => (env, adapter(input)))
    )

  def makeContext(input: Input): RIO[StartupEnv, StageContext[Env, State, Params]] =
    recipe.provideSome[StartupEnv](env => (env, input))

  def derivesParamsWith[Input1 <: Input, Params1](
    func: Input1 => Params1
  ): ContextSetup[StartupEnv, Input1, Env, State, Params1] =
    ContextSetup.CreateFromRecipe[StartupEnv, Input1, Env, State, Params1](
      ZIO.accessM[(StartupEnv, Input1)] { case (_, input) =>
        self.recipe.flatMap(ctx => ZIO.effect(ctx.updateParams(func(input))))
      }
    )

  /**
   * The effect that can be used to build the `StepContext`
   */
  def recipe: ZIO[(StartupEnv, Input), Throwable, StageContext[Env, State, Params]]

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
  def parseCommandLineWith[CmdLine <: Input, Params1](parse: CmdLine => Params1)(implicit
    @silent ev: CmdLine <:< List[String]
  ): ContextSetup[StartupEnv, CmdLine, Env, State, Params1] =
    ContextSetup[StartupEnv, CmdLine, Env, State, Params1](cmdLine =>
      self.recipe
        .flatMap(context => ZIO.effect(context.updateParams(parse(cmdLine))))
        .provideSome[StartupEnv](env => (env, cmdLine))
    )
}

object ContextSetup {

  def apply[StartupEnv, Input, Env, State, Params](
    f: Input => RIO[StartupEnv, StageContext[Env, State, Params]]
  ): ContextSetup[StartupEnv, Input, Env, State, Params] = Create[StartupEnv, Input, Env, State, Params](f)

  /**
   * Create a `ContextSetup` from a setup function (which is potentially side-effecting).
   */
  def create[Input, Env, State, Params](
    setupFunc: Input => StageContext[Env, State, Params]
  ): ContextSetup[Any, Input, Env, State, Params] =
    ContextSetup { input: Input => ZIO.effect(setupFunc(input)) }

  /**
   * Create an instance of the default StepContextConfig.
   * This configuration only requires ZIO's ZEnv, and accepts command line args (as a list of strings).
   */
  val default: ContextSetup[ZEnv, List[String], ZEnv, Any, Any] = ContextSetup { _: List[String] =>
    ZIO.access[ZEnv](StageContext.fromEnvironment)
  }

  def deriveParams[Input, Params](
    f: Input => Params
  ): ContextSetup[Any, Input, Any, Any, Params] =
    ContextSetup { input =>
      ZIO
        .accessM[(Any, Input)] { case (env, input) =>
          ZIO.effect(StageContext(environment = env, state = (), params = f(input)))
        }
        .provide(((), input))
    }

  val empty: ContextSetup[Any, Any, Any, Any, Any] = ContextSetup { _: Any =>
    ZIO.succeed(StageContext.any)
  }

  /**
   * Create a context setup from an effectual function that parses a command line.
   */
  def forCommandLineApp[StartupEnv <: Has[_], Params](
    func: List[String] => RIO[StartupEnv, Params]
  ): ContextSetup[StartupEnv, List[String], StartupEnv, Any, Params] =
    ContextSetup[StartupEnv, List[String], StartupEnv, Any, Params]((cmdLineArgs: List[String]) =>
      ZIO.accessM[StartupEnv] { env =>
        func(cmdLineArgs).map(params => StageContext(environment = env, state = (), params = params))
      }
    )

  /**
   * Creates a context setup that uses its initial startup requirements as the environment of the created context.
   */
  def givenEnvironmentAtStartup[R]: ContextSetup[R, Any, R, Any, Any] = ContextSetup { _: Any =>
    ZIO.access[R](env => StageContext(environment = env, state = (), params = ()))
  }

  def requiresEnvironmentOfType[R]: ContextSetup[R, Any, R, Any, Any] =
    new ContextSetup[R, Any, R, Any, Any] {

      /**
       * The effect that can be used to build the `StepContext`
       */
      def recipe: ZIO[(R, Any), Throwable, StageContext[R, Any, Any]] = ZIO.access[(R, Any)] { case (env, _) =>
        StageContext.fromEnvironment(env)
      }
    }

  /**
   * Creates a new context setup that requires input of the provided type
   */
  def requiresInputOfType[Input]: ContextSetup[Any, Input, Any, Any, Any] = ContextSetup { _: Input =>
    ZIO.succeed(StageContext(environment = (), state = (), params = ()))
  }

  def uses[StartupEnv]: ContextSetup[StartupEnv, Any, StartupEnv, Any, Any] = ContextSetup { _: Any =>
    ZIO.access[StartupEnv](StageContext.fromEnvironment)
  }

  //def make[StartupEnv, Input, Env, State, Params](effect:ZIO[Input, Throwable, StepContext[Env, State, Params]])
  //def make[R, StartupEnv, Input, Env, State, Params](effect:ZIO[R, Throwable, StepContext[Env, State, Params]]) = ???

  val withNoRequirements: ContextSetup[Any, Any, Any, Any, Any] = ContextSetup { _: Any =>
    ZIO.succeed(StageContext(environment = (), state = (), params = ()))
  }

  final case class CreateFromRecipe[StartupEnv, Input, Env, State, Params](
    recipe0: RIO[(StartupEnv, Input), StageContext[Env, State, Params]]
  ) extends ContextSetup[StartupEnv, Input, Env, State, Params] {

    /**
     * The effect that can be used to build the `StepContext`
     */
    def recipe: ZIO[(StartupEnv, Input), Throwable, StageContext[Env, State, Params]] = recipe0
  }

  final case class Create[StartupEnv, Input, Env, State, Params](
    f: Input => RIO[StartupEnv, StageContext[Env, State, Params]]
  ) extends ContextSetup[StartupEnv, Input, Env, State, Params] {

    /**
     * The effect that can be used to build the `StepContext`
     */
    def recipe: ZIO[(StartupEnv, Input), Throwable, StageContext[Env, State, Params]] =
      ZIO.accessM[(StartupEnv, Input)] { case (env, input) =>
        f(input).provide(env)
      }
  }
}
