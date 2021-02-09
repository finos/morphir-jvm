package morphir.flowz

import morphir.flowz.experimental.instrumentor.Instrumentor
import zio.clock.Clock

package object experimental {
  type FlowBaseEnv = Instrumentor with Clock
}
