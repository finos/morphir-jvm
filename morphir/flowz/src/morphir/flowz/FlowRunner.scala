package morphir.flowz

import zio.internal.Platform
import zio.{ ExecutionStrategy, Exit, FiberFailure, Has, Managed, RIO, Runtime, URIO, ZLayer }

final case class FlowRunner[InitialState, Msg, R <: Has[_], E](
  executor: FlowExecutor[InitialState, Msg, R, E],
  initialize: RIO[FlowBaseEnv, (InitialState, Msg)],
  bootstrap: ZLayer[FlowArgs, Nothing, FlowBaseEnv],
  platform: Platform = Platform.makeDefault()
) { self =>
  lazy val runtime: Runtime[Unit] = Runtime((), platform)

  /**
   * Runs the flow, producing the execution results.
   */
  def run(flow: ExecutableFlow[InitialState, Msg, R, E]): URIO[FlowBaseEnv, ExecutedFlow[E]] =
    executor.run(flow, ExecutionStrategy.ParallelN(4))(initialize).timed.flatMap { case (duration, results) =>
      //reporter(duration, results).as(results)
      ???
    }

  /**
   * An unsafe, synchronous run of the specified flow.
   */
  def unsafeRun(flow: ExecutableFlow[InitialState, Msg, R, E])(flowArgs: FlowArgs): ExecutedFlow[E] = {
    val completeBootstrap = ZLayer.succeedMany(flowArgs) >>> bootstrap
    self.runtime.unsafeRun(run(flow).provideLayer(completeBootstrap))
  }

  /**
   * An unsafe, asynchronous run of the specified flow.
   */
  def unsafeRunAsync(
    flow: ExecutableFlow[InitialState, Msg, R, E]
  )(flowArgs: FlowArgs)(
    k: ExecutedFlow[E] => Unit
  ): Unit = {
    val completeBootstrap = ZLayer.succeedMany(flowArgs) >>> bootstrap
    runtime.unsafeRunAsync(run(flow).provideLayer(completeBootstrap)) {
      case Exit.Success(v) => k(v)
      case Exit.Failure(c) => throw FiberFailure(c)
    }
  }

  /**
   * An unsafe, synchronous run of the specified flow.
   */
  def unsafeRunSync(
    flow: ExecutableFlow[InitialState, Msg, R, E]
  )(flowArgs: FlowArgs): Exit[Nothing, ExecutedFlow[E]] = {
    val completeBootstrap = ZLayer.succeedMany(flowArgs) >>> bootstrap
    self.runtime.unsafeRunSync(run(flow).provideLayer(completeBootstrap))
  }

  def withPlatform(f: Platform => Platform): FlowRunner[InitialState, Msg, R, E] =
    copy(platform = f(platform))

  private[flowz] def buildRuntime(flowArgs: FlowArgs): Managed[Nothing, Runtime[FlowBaseEnv]] = {
    val completeBootstrap = ZLayer.succeedMany(flowArgs) >>> bootstrap
    completeBootstrap.toRuntime(platform)
  }
}
