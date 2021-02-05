package morphir.flowz

import zio.Has
import zio.logging.{ Appender, Logger }

package object instrumentation {
  type InstrumentationAppender = Appender[InstrumentationEvent]
  type InstrumentationLogger   = Logger[InstrumentationEvent]
  type InstrumentationLogging  = Has[InstrumentationLogger]
}
