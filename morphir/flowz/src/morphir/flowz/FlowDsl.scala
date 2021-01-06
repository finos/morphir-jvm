package morphir.flowz

import zio._

trait FlowDsl {
  def flow(name: String): Flow.Builder[Any, Any, Nothing, Nothing, Nothing, Nothing, Any] =
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

    def builder(name: String): Builder[Any, Any, Nothing, Nothing, Nothing, Nothing, Any] =
      apply(name)

    object Phases {
      type Setup
      type DefineStages
    }

    def apply(name: String): Builder[Any, Any, Nothing, Nothing, Nothing, Nothing, Any] =
      Builder(name)

    sealed trait Builder[-StartupEnv, -Input, +Env, +State, +Params, +Output, Phase] { self =>

      def setup[Input1 <: Input, Env1 >: Env, State1 >: State, Params1 >: Params](
        setupFunc: Input1 => StepContext[Env1, State1, Params1]
      ): Builder[StartupEnv, Input1, Env1, State1, Params1, Output, Phase with Phases.Setup]

      def setupWithEffect[StartupEnv1 <: StartupEnv, Input1 <: Input, Env1 >: Env, State1 >: State, Params1 >: Params](
        effectualSetupFunc: Input1 => RIO[StartupEnv1, StepContext[Env1, State1, Params1]]
      ): Builder[StartupEnv1, Input1, Env1, State1, Params1, Output, Phase with Phases.Setup]

      def stages[Input1 <: Input, Env1 >: Env, State1 >: State, Params1 >: Params, Output1](
        step: Step[State1, _, Env1, Params1, Throwable, Output1]
      ): Builder[StartupEnv, Input1, Env1, State1, Params1, Output1, Phase with Phases.DefineStages]

      def build(implicit
        ev: Phase <:< Phases.Setup with Phases.DefineStages
      ): Flow[StartupEnv, Input, Output]
    }

    object Builder {
      def apply(name: String): Builder[Any, Any, Nothing, Nothing, Nothing, Nothing, Any] =
        FlowBuilder(name, None, None)

      private sealed case class FlowBuilder[StartupEnv, Input, Env0, State0, Params0, +Output, Phase](
        name: String,
        setupEffect: Option[RIO[(StartupEnv, Input), StepContext[Env0, State0, Params0]]],
        step: Option[Step[State0, _, Env0, Params0, Throwable, Output]]
      ) extends Builder[StartupEnv, Input, Env0, State0, Params0, Output, Phase] { self =>

        def build(implicit
          ev: Phase <:< Phases.Setup with Phases.DefineStages
        ): Flow[StartupEnv, Input, Output] =
          new Flow[StartupEnv, Input, Output] {
            type Env          = Env0
            type InitialState = State0
            type Params       = Params0

            def name: String = self.name

            def context(input: Input): RIO[StartupEnv, StepContext[Env, InitialState, Params]] =
              self.setupEffect.get.provideSome[StartupEnv](startupEnv => (startupEnv, input))

            def step: Step[InitialState, _, Env, Params, Throwable, Output] = self.step.get
          }

        def setup[Input1 <: Input, Env1 >: Env0, State1 >: State0, Params1 >: Params0](
          setupFunc: Input1 => StepContext[Env1, State1, Params1]
        ): Builder[StartupEnv, Input1, Env1, State1, Params1, Output, Phase with Phases.Setup] = {
          val setupEffect = ZIO.environment[(StartupEnv, Input1)].mapEffect { case (_, input) => setupFunc(input) }
          FlowBuilder[StartupEnv, Input1, Env1, State1, Params1, Output, Phase with Phases.Setup](
            name = self.name,
            setupEffect = Some(setupEffect),
            step = self.step.map(_.asInstanceOf[Step[State1, _, Env1, Params1, Throwable, Output]])
          )
        }

        def setupWithEffect[
          StartupEnv1 <: StartupEnv,
          Input1 <: Input,
          Env1 >: Env0,
          State1 >: State0,
          Params1 >: Params0
        ](
          effectualSetupFunc: Input1 => RIO[StartupEnv1, StepContext[Env1, State1, Params1]]
        ): Builder[StartupEnv1, Input1, Env1, State1, Params1, Output, Phase with Phases.Setup] = {
          val setupEffect = ZIO.environment[(StartupEnv1, Input1)].flatMap { case (startupEnv, input) =>
            effectualSetupFunc(input).provide(startupEnv)
          }
          FlowBuilder[StartupEnv1, Input1, Env1, State1, Params1, Output, Phase with Phases.Setup](
            name = self.name,
            setupEffect = Some(setupEffect),
            step = self.step.map(_.asInstanceOf[Step[State1, _, Env1, Params1, Throwable, Output]])
          )
        }

        def stages[Input1 <: Input, Env1 >: Env0, State1 >: State0, Params1 >: Params0, Output1](
          step: Step[State1, _, Env1, Params1, Throwable, Output1]
        ): Builder[StartupEnv, Input1, Env1, State1, Params1, Output1, Phase with Phases.DefineStages] =
          copy(step = Some(step))

      }

    }

  }

  object demo {
    flow("sum-flow")
      .setup((args: List[Int]) => StepContext.fromParams(args))
      .stages(Step.fromFunction { items: List[Int] => items.sum })
      .build
      .run(List(1, 2, 3))
  }
}
