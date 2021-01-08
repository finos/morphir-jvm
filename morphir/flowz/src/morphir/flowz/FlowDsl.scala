package morphir.flowz

import zio._

trait FlowDsl {
  def flow(name: String): Flow.Builder[Any, Any, Nothing, Unit, Nothing, Nothing, Any] =
    Flow.builder(name)

  abstract class Flow[-StartupEnv, -Input, +Output] {
    type Env
    type InitialState
    type Params

    def name: String
    def context(input: Input): RIO[StartupEnv, StepContext[Env, InitialState, Params]]
    def step: Step[InitialState, _, Env, Params, Throwable, Output]
    def run(input: Input): RIO[StartupEnv, Output] =
      for {
        stepContext <- context(input)
        result      <- step.run(stepContext)
      } yield result.value
  }

  object Flow {
    type Aux[-StartupEnv, -Input, Env0, State0, Params0, +Output] = Flow[StartupEnv, Input, Output] {
      type Env          = Env0
      type InitialState = State0
      type Params       = Params0
    }

    def builder(name: String): Builder[Any, Any, Nothing, Unit, Nothing, Nothing, Any] =
      apply(name)

    object BuilderPhase {
      type Setup
      type DefineStages
      type Report
    }

    def apply(name: String): Builder[Any, Any, Nothing, Unit, Nothing, Nothing, Any] =
      Builder(name)

    sealed trait Builder[-StartupEnv, -Input, Env, State, Params, +Output, Phase] { self =>

      def setup[Input1 <: Input, Env1, State1, Params1](
        setupFunc: Input1 => StepContext[Env1, State1, Params1]
      )(implicit
        ev: Any <:< Phase
      ): Builder[StartupEnv, Input1, Env1, State1, Params1, Output, Phase with BuilderPhase.Setup]

      def setupWithEffect[StartupEnv1 <: StartupEnv, Input1 <: Input, Env1, State1, Params1](
        effectualSetupFunc: Input1 => RIO[StartupEnv1, StepContext[Env1, State1, Params1]]
      )(implicit
        ev: Any <:< Phase
      ): Builder[StartupEnv1, Input1, Env1, State1, Params1, Output, Phase with BuilderPhase.Setup]

      def stages[Output1](
        step: Step[State, _, Env, Params, Throwable, Output1]
      ): Builder[StartupEnv, Input, Env, State, Params, Output1, Phase with BuilderPhase.DefineStages]

      def build(implicit
        ev: Phase <:< BuilderPhase.Setup with BuilderPhase.DefineStages
      ): Flow[StartupEnv, Input, Output]

      def report(
        f: Output => ZIO[Env, Throwable, Any]
      ): Builder[StartupEnv, Input, Env, State, Params, Output, Phase with BuilderPhase.Report]
    }

    object Builder {
      def apply(name: String): Builder[Any, Any, Nothing, Unit, Nothing, Nothing, Any] =
        FlowBuilder(name, None, None, None)

      private sealed case class FlowBuilder[StartupEnv, Input, Env, StateIn, Params, Output, Phase](
        name: String,
        setupEffect: Option[RIO[(StartupEnv, Input), StepContext[Env, StateIn, Params]]],
        step: Option[Step[StateIn, _, Env, Params, Throwable, Output]],
        report: Option[Output => ZIO[Env, Throwable, Any]]
      ) extends Builder[StartupEnv, Input, Env, StateIn, Params, Output, Phase] { self =>

        def build(implicit
          ev: Phase <:< BuilderPhase.Setup with BuilderPhase.DefineStages
        ): Flow[StartupEnv, Input, Output] = {
          type Env0    = Env
          type Params0 = Params
          new Flow[StartupEnv, Input, Output] {
            type Env          = Env0
            type InitialState = StateIn
            type Params       = Params0

            def name: String = self.name

            def context(input: Input): RIO[StartupEnv, StepContext[Env, InitialState, Params]] =
              self.setupEffect.get.provideSome[StartupEnv](startupEnv => (startupEnv, input))

            def step: Step[InitialState, _, Env, Params, Throwable, Output] = self.step.get.tap { case (_, output) =>
              self.report.fold[ZIO[Env, Throwable, Any]](ZIO.unit)(_(output))
            }
          }
        }

        def setup[Input1 <: Input, Env1, State1, Params1](
          setupFunc: Input1 => StepContext[Env1, State1, Params1]
        )(implicit
          ev: Any <:< Phase
        ): Builder[StartupEnv, Input1, Env1, State1, Params1, Output, Phase with BuilderPhase.Setup] = {
          val setupEffect = ZIO.environment[(StartupEnv, Input1)].mapEffect { case (_, input) => setupFunc(input) }
          FlowBuilder[StartupEnv, Input1, Env1, State1, Params1, Output, Phase with BuilderPhase.Setup](
            name = self.name,
            setupEffect = Some(setupEffect),
            step = None,
            report = None
          )
        }

        def setupWithEffect[StartupEnv1 <: StartupEnv, Input1 <: Input, Env1, State1, Params1](
          effectualSetupFunc: Input1 => RIO[StartupEnv1, StepContext[Env1, State1, Params1]]
        )(implicit
          ev: Any <:< Phase
        ): Builder[StartupEnv1, Input1, Env1, State1, Params1, Output, Phase with BuilderPhase.Setup] = {
          val setupEffect = ZIO.environment[(StartupEnv1, Input1)].flatMap { case (startupEnv, input) =>
            effectualSetupFunc(input).provide(startupEnv)
          }
          FlowBuilder[StartupEnv1, Input1, Env1, State1, Params1, Output, Phase with BuilderPhase.Setup](
            name = self.name,
            setupEffect = Some(setupEffect),
            step = None,
            report = None
          )
        }

        def stages[Output1](
          step: Step[StateIn, _, Env, Params, Throwable, Output1]
        ): Builder[StartupEnv, Input, Env, StateIn, Params, Output1, Phase with BuilderPhase.DefineStages] =
          FlowBuilder[StartupEnv, Input, Env, StateIn, Params, Output1, Phase with BuilderPhase.DefineStages](
            name = self.name,
            setupEffect = self.setupEffect,
            step = Some(step),
            report = None
          )

        def report(
          f: Output => ZIO[Env, Throwable, Any]
        ): Builder[StartupEnv, Input, Env, StateIn, Params, Output, Phase with BuilderPhase.Report] =
          FlowBuilder[StartupEnv, Input, Env, StateIn, Params, Output, Phase with BuilderPhase.Report](
            name = self.name,
            step = self.step,
            setupEffect = self.setupEffect,
            report = self.report.map(g => (o: Output) => f(o) *> g(o)) orElse Some(f)
          )

      }

    }

  }
}

object demo extends zio.App {
  import morphir.flowz.api._

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    flow("sum-flow")
      .setupWithEffect((args: List[Int]) =>
        ZIO.access[console.Console](env => StepContext(environment = env, state = (), params = args))
      )
      .stages(
        stage((_: Any, items: List[Int]) => Step.succeed(items.sum))
      )
      .report(res => console.putStrLn(s"Result: $res"))
      .build
      .run(List(1, 2, 3))
      .exitCode
}
