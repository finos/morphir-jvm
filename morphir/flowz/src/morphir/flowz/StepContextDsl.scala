package morphir.flowz

import zio._

sealed trait StepContextDsl {

  trait StepContextConfig[-StartupEnv, -Input, +Env, +State, +Params] {
    def build(input: Input): RIO[StartupEnv, StepContext[Env, State, Params]]
  }

  object StepContextConfig {

    def apply[StartupEnv, Input, Env, State, Params](
      f: Input => RIO[StartupEnv, StepContext[Env, State, Params]]
    ): StepContextConfig[StartupEnv, Input, Env, State, Params] =
      new StepContextConfig[StartupEnv, Input, Env, State, Params] {
        def build(input: Input): RIO[StartupEnv, StepContext[Env, State, Params]] =
          ZIO.accessM[StartupEnv](env => f(input).provide(env))
      }

    val default: StepContextConfig[ZEnv, Any, ZEnv, Any, Any] = StepContextConfig { _: Any =>
      ZIO.access[ZEnv](StepContext.fromEnvironment)
    }

    def deriveParams[Input, Params](f: Input => Params): StepContextConfig[Any, Input, Any, Any, Any] =
      StepContextConfig { input =>
        ZIO.effect(StepContext.fromParams(f(input)))
      }

    val withNoRequirements: StepContextConfig[Any, Any, Any, Any, Any] = StepContextConfig { _: Any =>
      ZIO.succeed(StepContext(environment = (), state = (), params = ()))
    }
  }

}
