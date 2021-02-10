package morphir.flowz

import morphir.flowz.experimental.instrumentor.Instrumentor
import zio.{ ExitCode, URIO, ZIO, ZManaged }
import zio.clock.Clock
import zio.duration.Duration

package object experimental {
  type FlowBaseEnv                                      = Instrumentor with Clock
  type ExecutableFlow[-InitialState, -InputMsg, -R, +E] = Flow[InitialState, Any, InputMsg, R, E, ExitCode]

  def process[SIn, SOut, In, R, Err, Out](label: String)(
    children: Flow[SIn, SOut, In, R, Err, Out]*
  ): Flow[SIn, SOut, In, R, Err, Out] =
    Flow.process(label, children = ZManaged.succeed(children.toVector))

  def step[SIn, SOut, In, R, Err, Out](label: String)(
    behavior: Step[SIn, SOut, In, R, Err, Out]
  ): Flow[SIn, SOut, In, R, Err, Out] = Flow.step(label, behavior, PropertyMap.empty)

  /**
   * A `FlowReporter[E]` is capable of reporting flow execution results with error type `E`.
   */
  type FlowReporter[-E] = (Duration, ExecutedFlow[E]) => URIO[Instrumentor, Unit]
  object FlowReporter {

    /**
     * A `FlowReporter` that does nothing.
     */
    val silent: FlowReporter[Any] = (_, _) => ZIO.unit
  }
}
