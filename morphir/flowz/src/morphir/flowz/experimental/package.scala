package morphir.flowz

import morphir.flowz.experimental.instrumentor.Instrumentor
import zio.ZManaged
import zio.clock.Clock

package object experimental {
  type FlowBaseEnv = Instrumentor with Clock
  def process[SIn, SOut, In, R, Err, Out](label: String)(
    children: Flow[SIn, SOut, In, R, Err, Out]*
  ): Flow[SIn, SOut, In, R, Err, Out] =
    Flow.process(label, children = ZManaged.succeed(children.toVector))

  def step[SIn, SOut, In, R, Err, Out](label: String)(
    behavior: Step[SIn, SOut, In, R, Err, Out]
  ): Flow[SIn, SOut, In, R, Err, Out] = Flow.step(label, behavior, PropertyMap.empty)
}
