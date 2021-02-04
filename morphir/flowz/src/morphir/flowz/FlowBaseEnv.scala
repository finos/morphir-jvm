package morphir.flowz

import morphir.flowz.instrumentor.Instrumentor
import zio.Layer
import zio.console.Console
import zio.clock.Clock

object FlowBaseEnv {
  val default: Layer[Nothing, FlowBaseEnv] =
    Console.live ++ Clock.live >+> Instrumentor.default
}
