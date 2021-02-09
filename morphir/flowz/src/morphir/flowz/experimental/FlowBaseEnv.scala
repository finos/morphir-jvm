package morphir.flowz.experimental

import instrumentor.Instrumentor
import zio.Layer
import zio.clock.Clock
import zio.console.Console

object FlowBaseEnv {
  val default: Layer[Nothing, FlowBaseEnv] =
    Console.live ++ Clock.live >+> Instrumentor.console()
}
