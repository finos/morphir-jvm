package morphir.flowz

import zio.internal.Platform
import zio.{ Has, RIO, Runtime, URIO, ZLayer }

final case class FlowRunner[InitialState, Msg, R <: Has[_], E](
  executor: FlowExecutor[R, E],
  initialize: RIO[FlowBaseEnv, (InitialState, Msg)],
  bootstrap: ZLayer[FlowArgs, Nothing, FlowBaseEnv],
  platform: Platform = Platform.makeDefault()
) { self =>
  lazy val runtime: Runtime[Unit] = Runtime((), platform)

  /**
   * Runs the flow, producing the execution results.
   */
  def run(flow: RunnableFlow[InitialState, Msg, R, E]): URIO[FlowBaseEnv, ExecutedFlow[E]] = ???

  def unsafeRun(flow: RunnableFlow[InitialState, Msg, R, E])(flowArgs: FlowArgs): ExecutedFlow[E] = {
    val finalBootstrap = ZLayer.succeedMany(flowArgs) >>> bootstrap
    self.runtime.unsafeRun(run(flow).provideLayer(finalBootstrap))
  }
}
