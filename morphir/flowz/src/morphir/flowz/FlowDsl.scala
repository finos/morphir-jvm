package morphir.flowz

import zio._
import com.github.ghik.silencer.silent
trait FlowDsl {
  def flow[StartupEnv, Input, Env, StateIn, Params](
    name: String,
    setup: => ContextSetup[StartupEnv, Input, Env, StateIn, Params]
  ): Flow.Builder[StartupEnv, Input, Env, StateIn, Params, Any, Flow.BuilderPhase.Setup] =
    Flow.builder(name)(setup)

  abstract class Flow[-StartupEnv, -Input, +Output] {
    type Env
    type InitialState
    type Params

    def name: String
    def context(input: Input): RIO[StartupEnv, ActContext[Env, InitialState, Params]]
    def step: Act[InitialState, _, Env, Params, Throwable, Output]
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

    def builder[StartupEnv, Input, Env, StateIn, Params](name: String)(
      setup: => ContextSetup[StartupEnv, Input, Env, StateIn, Params]
    ): Builder[StartupEnv, Input, Env, StateIn, Params, Any, BuilderPhase.Setup] = apply(name)(setup)

    object BuilderPhase {
      type Setup
      type DefineStages
      type Report
    }

    def apply[StartupEnv, Input, Env, StateIn, Params](name: String)(
      setup: => ContextSetup[StartupEnv, Input, Env, StateIn, Params]
    ): Builder[StartupEnv, Input, Env, StateIn, Params, Any, BuilderPhase.Setup] =
      Builder(name)(setup)

    sealed abstract class Builder[-StartupEnv, -Input, Env, State, Params, Output, Phase] { self =>

      protected def name: String
      protected def step: Option[Act[State, _, Env, Params, Throwable, Output]]
      protected def contextSetup: ContextSetup[StartupEnv, Input, Env, State, Params]
      protected def report: Option[Output => ZIO[Env, Throwable, Any]]

      final def setup[StartupEnv1 <: StartupEnv, Input1, Env1, State1, Params1](
        contextSetup: => ContextSetup[StartupEnv1, Input1, Env1, State1, Params1]
      ): Builder[StartupEnv1, Input1, Env1, State1, Params1, Output, Phase with BuilderPhase.Setup] =
        Builder[StartupEnv1, Input1, Env1, State1, Params1, Output, Phase with BuilderPhase.Setup](
          name = self.name,
          contextSetup = contextSetup,
          step = None,
          report = None
        )

      final def setup[StartupEnv0 <: StartupEnv, StartupEnv1, Input1, Env1, State1, Params1](
        configure: ContextSetup[StartupEnv0, Input, Env, State, Params] => ContextSetup[
          StartupEnv1,
          Input1,
          Env1,
          State1,
          Params1
        ]
      ): Builder[StartupEnv1, Input1, Env1, State1, Params1, Output, Phase with BuilderPhase.Setup] =
        Builder[StartupEnv1, Input1, Env1, State1, Params1, Output, Phase with BuilderPhase.Setup](
          name = self.name,
          contextSetup = contextSetup.configure(configure),
          step = None,
          report = None
        )

      final def stages[Output1](
        step: Act[State, _, Env, Params, Throwable, Output1]
      ): Builder[StartupEnv, Input, Env, State, Params, Output1, Phase with BuilderPhase.DefineStages] =
        Builder[StartupEnv, Input, Env, State, Params, Output1, Phase with BuilderPhase.DefineStages](
          name = self.name,
          contextSetup = self.contextSetup,
          step = Some(step),
          report = None
        )

      def build(implicit
        ev: Phase <:< BuilderPhase.Setup with BuilderPhase.DefineStages
      ): Flow[StartupEnv, Input, Output]

      final def report(
        f: Output => ZIO[Env, Throwable, Any]
      )(implicit
        @silent("never used") ev: Phase <:< BuilderPhase.DefineStages
      ): Builder[StartupEnv, Input, Env, State, Params, Output, Phase with BuilderPhase.Report] =
        Builder[StartupEnv, Input, Env, State, Params, Output, Phase with BuilderPhase.Report](
          name = self.name,
          contextSetup = self.contextSetup,
          step = self.step,
          report = self.report.map(g => (o: Output) => f(o) *> g(o)) orElse Some(f)
        )
    }

    object Builder {
      def apply[StartupEnv, Input, Env, StateIn, Params](
        name: String
      )(
        setup: => ContextSetup[StartupEnv, Input, Env, StateIn, Params]
      ): Builder[StartupEnv, Input, Env, StateIn, Params, Any, BuilderPhase.Setup] =
        FlowBuilder(
          name = name,
          step = None,
          contextSetup = setup,
          report = None
        )

      private def apply[StartupEnv, Input, Env, StateIn, Params, Output, Phase](
        name: String,
        step: Option[Act[StateIn, _, Env, Params, Throwable, Output]],
        contextSetup: ContextSetup[StartupEnv, Input, Env, StateIn, Params],
        report: Option[Output => ZIO[Env, Throwable, Any]]
      ): Builder[StartupEnv, Input, Env, StateIn, Params, Output, Phase] =
        FlowBuilder(
          name = name,
          step = step,
          contextSetup = contextSetup,
          report = report
        )

      private sealed case class FlowBuilder[StartupEnv, Input, Env, StateIn, Params, Output, Phase](
        name: String,
        step: Option[Act[StateIn, _, Env, Params, Throwable, Output]],
        contextSetup: ContextSetup[StartupEnv, Input, Env, StateIn, Params],
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

            def context(input: Input): RIO[StartupEnv, ActContext[Env, InitialState, Params]] =
              self.contextSetup.makeContext(input)

            def step: Act[InitialState, _, Env, Params, Throwable, Output] = self.step.get.tap { case (_, output) =>
              self.report.fold[ZIO[Env, Throwable, Any]](ZIO.unit)(_(output))
            }
          }
        }

      }

    }

  }
}

object demo extends zio.App {
  import morphir.flowz.api._

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    flow(
      name = "sum-flow",
      setup = ContextSetup.forCommandLineApp(args =>
        ZIO.accessM[console.Console](_ => ZIO.collectAllSuccesses(args.map(entry => ZIO.effect(entry.toInt))))
      )
    )
      .stages(
        stage((_: Any, items: List[Int]) => Act.succeed(items.sum))
      )
      .report(res => console.putStrLn(s"Result: $res"))
      .build
      .run(List("1", "2", "3"))
      .exitCode
}
