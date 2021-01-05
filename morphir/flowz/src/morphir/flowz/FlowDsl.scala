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

    sealed trait Builder[-StartupEnv, -Input, +Env, +State, +Params, +Output, Phase] {

      def setup[Input1 <: Input, Env1 >: Env, State1 >: State, Params1 >: Params](
        func: Input1 => StepContext[Env1, State1, Params1]
      ): Builder[StartupEnv, Input1, Env1, State1, Params1, Output, Phase with Phases.Setup]

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

      private sealed case class FlowBuilder[StartupEnv, Input, Env, State, Params, +Output, Phase](
        name: String,
        setupEffect: Option[RIO[(StartupEnv, Input), StepContext[Env, State, Params]]],
        step: Option[Step[State, _, Env, Params, Throwable, Output]]
      ) extends Builder[StartupEnv, Input, Env, State, Params, Output, Phase] { self =>

        def build(implicit
          ev: Phase <:< Phases.Setup with Phases.DefineStages
        ): Flow[StartupEnv, Input, Output] = ???

        def setup[Input1 <: Input, Env1 >: Env, State1 >: State, Params1 >: Params](
          func: Input1 => StepContext[Env1, State1, Params1]
        ): Builder[StartupEnv, Input1, Env1, State1, Params1, Output, Phase with Phases.Setup] = {
          val setupEffect = ZIO.environment[(StartupEnv, Input1)].mapEffect { case (_, input) => func(input) }
          FlowBuilder[StartupEnv, Input1, Env1, State1, Params1, Output, Phase with Phases.Setup](
            name = self.name,
            setupEffect = Some(setupEffect),
            step = self.step.map(_.asInstanceOf[Step[State1, _, Env1, Params1, Throwable, Output]])
          )
        }

        def stages[Input1 <: Input, Env1 >: Env, State1 >: State, Params1 >: Params, Output1](
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
