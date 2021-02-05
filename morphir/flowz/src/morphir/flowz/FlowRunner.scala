package morphir.flowz

import zio.internal.Platform
import zio._

final case class FlowRunner[+InitialState, Msg, R <: Has[_], E](
  executor: FlowExecutor[InitialState, Msg, R, E],
  platform: Platform = Platform.makeDefault().withReportFailure(_ => ()),
  reporter: FlowReporter[E] = FlowReporter.silent, //TODO: Make this a default one that actually does something
  bootstrap: Layer[Nothing, FlowBaseEnv] = FlowBaseEnv.default
) { self =>
  lazy val runtime: Runtime[Unit] = Runtime((), platform)

  /**
   * Runs the flow, producing the execution results.
   */
  def run(flow: ExecutableFlow[InitialState, Msg, R, E]): URIO[FlowBaseEnv, ExecutedFlow[E]] =
    executor.run(flow, ExecutionStrategy.ParallelN(4)).timed.flatMap { case (duration, results) =>
      reporter(duration, results).as(results)
    }

  /**
   * An unsafe, synchronous run of the specified flow.
   */
  def unsafeRun(flow: ExecutableFlow[InitialState, Msg, R, E]): ExecutedFlow[E] =
    self.runtime.unsafeRun(run(flow).provideLayer(bootstrap))

  /**
   * An unsafe, asynchronous run of the specified flow.
   */
  def unsafeRunAsync(flow: ExecutableFlow[InitialState, Msg, R, E])(k: ExecutedFlow[E] => Unit): Unit =
    runtime.unsafeRunAsync(run(flow).provideLayer(bootstrap)) {
      case Exit.Success(v) => k(v)
      case Exit.Failure(c) => throw FiberFailure(c)
    }

  /**
   * An unsafe, synchronous run of the specified flow.
   */
  def unsafeRunSync(
    flow: ExecutableFlow[InitialState, Msg, R, E]
  ): Exit[Nothing, ExecutedFlow[E]] =
    self.runtime.unsafeRunSync(run(flow).provideLayer(bootstrap))

  def withPlatform(f: Platform => Platform): FlowRunner[InitialState, Msg, R, E] =
    copy(platform = f(platform))

  private[flowz] def buildRuntime: Managed[Nothing, Runtime[FlowBaseEnv]] =
    bootstrap.toRuntime(platform)
}
