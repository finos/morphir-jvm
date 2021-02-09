package morphir.flowz
import morphir.flowz.instrumentation.{ InstrumentationEvent, InstrumentationLogger, InstrumentationLogging }
import zio.logging.{ LogContext, LogLevel }
import zio.{ Cause, URIO, ZIO }

/**
 * Provides accessors for using the InstrumentationLogger.
 */
object iLog {
  def apply(level: LogLevel)(event: => InstrumentationEvent): ZIO[InstrumentationLogging, Nothing, Unit] =
    InstrumentationLogging.log(level)(event)

  val context: URIO[InstrumentationLogging, LogContext] =
    InstrumentationLogging.context

  def debug(event: => InstrumentationEvent): ZIO[InstrumentationLogging, Nothing, Unit] =
    InstrumentationLogging.debug(event)

  def derive(f: LogContext => LogContext): ZIO[InstrumentationLogging, Nothing, InstrumentationLogger] =
    InstrumentationLogging.derive(f)

  def error(event: => InstrumentationEvent): ZIO[InstrumentationLogging, Nothing, Unit] =
    InstrumentationLogging.error(event)

  def error(event: => InstrumentationEvent, cause: Cause[Any]): ZIO[InstrumentationLogging, Nothing, Unit] =
    InstrumentationLogging.error(event, cause)

  def info(event: => InstrumentationEvent): ZIO[InstrumentationLogging, Nothing, Unit] =
    InstrumentationLogging.info(event)

  def locally[R <: InstrumentationLogging, E, A1](fn: LogContext => LogContext)(
    zio: ZIO[R, E, A1]
  ): ZIO[InstrumentationLogging with R, E, A1] =
    InstrumentationLogging.locally(fn)(zio)

  def locallyM[R <: InstrumentationLogging, E, A1](
    fn: LogContext => URIO[R, LogContext]
  )(zio: ZIO[R, E, A1]): ZIO[InstrumentationLogging with R, E, A1] =
    InstrumentationLogging.locallyM(fn)(zio)

  def throwable(event: => InstrumentationEvent, t: Throwable): ZIO[InstrumentationLogging, Nothing, Unit] =
    InstrumentationLogging.throwable(event, t)

  def trace(event: => InstrumentationEvent): ZIO[InstrumentationLogging, Nothing, Unit] =
    InstrumentationLogging.trace(event)

  def warn(event: => InstrumentationEvent): ZIO[InstrumentationLogging, Nothing, Unit] =
    InstrumentationLogging.warn(event)

}
