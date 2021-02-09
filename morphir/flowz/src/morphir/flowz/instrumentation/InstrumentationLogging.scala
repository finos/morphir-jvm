package morphir.flowz.instrumentation

import zio.clock.Clock
import zio.console.Console
import zio.logging.Logger.LoggerWithFormat
import zio.logging.Logging.addTimestamp
import zio.logging.{ LogAnnotation, LogAppender, LogContext, LogFormat, LogLevel }
import zio.{ Cause, FiberRef, Has, Layer, URIO, ZIO, ZLayer }

import java.nio.charset.{ Charset, StandardCharsets }
import java.nio.file.Path

object InstrumentationLogging {
  def console(
    logLevel: LogLevel = LogLevel.Info,
    format: LogFormat[InstrumentationEvent] = InstrumentationEvent.logFormats.coloredConsoleLogFormat
  ): ZLayer[Console with Clock, Nothing, InstrumentationLogging] =
    ZLayer.requires[Clock] ++
      LogAppender.console[InstrumentationEvent](
        logLevel,
        format
      ) >+> InstrumentationLogging.make >>> InstrumentationLogging.modifyLoggerM(addTimestamp[InstrumentationEvent])

  def consoleErr(
    logLevel: LogLevel = LogLevel.Info,
    format: LogFormat[InstrumentationEvent] = InstrumentationEvent.logFormats.simpleConsoleLogFormat
  ): ZLayer[Console with Clock, Nothing, InstrumentationLogging] =
    ZLayer.requires[Clock] ++
      LogAppender.consoleErr[InstrumentationEvent](
        logLevel,
        format
      ) >+> make >>> modifyLoggerM(addTimestamp[InstrumentationEvent])

  val context: URIO[InstrumentationLogging, LogContext] =
    ZIO.accessM[InstrumentationLogging](_.get.logContext)

  def debug(event: => InstrumentationEvent): ZIO[InstrumentationLogging, Nothing, Unit] =
    ZIO.accessM[InstrumentationLogging](_.get.debug(event))

  def derive(f: LogContext => LogContext): ZIO[InstrumentationLogging, Nothing, InstrumentationLogger] =
    ZIO.access[InstrumentationLogging](_.get.derive(f))

  def error(event: => InstrumentationEvent): ZIO[InstrumentationLogging, Nothing, Unit] =
    ZIO.accessM[InstrumentationLogging](_.get.error(event))

  def error(event: => InstrumentationEvent, cause: Cause[Any]): ZIO[InstrumentationLogging, Nothing, Unit] =
    ZIO.accessM[InstrumentationLogging](_.get.error(event, cause))

  def file(
    destination: Path,
    charset: Charset = StandardCharsets.UTF_8,
    autoFlushBatchSize: Int = 1,
    bufferedIOSize: Option[Int] = None,
    logLevel: LogLevel = LogLevel.Info,
    format: LogFormat[InstrumentationEvent] = InstrumentationEvent.logFormats.simpleConsoleLogFormat
  ): ZLayer[Console with Clock, Throwable, InstrumentationLogging] =
    (ZLayer.requires[Clock] ++
      LogAppender
        .file[InstrumentationEvent](destination, charset, autoFlushBatchSize, bufferedIOSize, format)
        .map(appender => Has(appender.get.filter((ctx, _) => ctx.get(LogAnnotation.Level) >= logLevel)))
      >+> make >>> modifyLoggerM(addTimestamp[InstrumentationEvent]))

  def fileAsync(
    destination: Path,
    charset: Charset = StandardCharsets.UTF_8,
    autoFlushBatchSize: Int = 32,
    bufferedIOSize: Option[Int] = Some(8192),
    logLevel: LogLevel = LogLevel.Info,
    format: LogFormat[InstrumentationEvent] = InstrumentationEvent.logFormats.simpleConsoleLogFormat
  ): ZLayer[Console with Clock, Throwable, InstrumentationLogging] =
    (ZLayer.requires[Clock] ++
      (LogAppender
        .file[InstrumentationEvent](destination, charset, autoFlushBatchSize, bufferedIOSize, format)
        .map(appender => Has(appender.get.filter((ctx, _) => ctx.get(LogAnnotation.Level) >= logLevel)))
        >>> LogAppender.async(autoFlushBatchSize))
      >+> make >>> modifyLoggerM(addTimestamp[InstrumentationEvent]))

  val ignore: Layer[Nothing, InstrumentationLogging] =
    LogAppender.ignore[InstrumentationEvent] >>> make

  def info(event: => InstrumentationEvent): ZIO[InstrumentationLogging, Nothing, Unit] =
    ZIO.accessM[InstrumentationLogging](_.get.info(event))

  def log(level: LogLevel)(event: => InstrumentationEvent): ZIO[InstrumentationLogging, Nothing, Unit] =
    ZIO.accessM[InstrumentationLogging](_.get.log(level)(event))

  def locally[A, R <: InstrumentationLogging, E, A1](fn: LogContext => LogContext)(
    zio: ZIO[R, E, A1]
  ): ZIO[InstrumentationLogging with R, E, A1] =
    ZIO.accessM(_.get.locally(fn)(zio))

  def locallyM[A, R <: InstrumentationLogging, E, A1](
    fn: LogContext => URIO[R, LogContext]
  )(zio: ZIO[R, E, A1]): ZIO[InstrumentationLogging with R, E, A1] =
    ZIO.accessM(_.get.locallyM(fn)(zio))

  def make: ZLayer[InstrumentationAppender, Nothing, InstrumentationLogging] =
    ZLayer.fromFunctionM((appender: InstrumentationAppender) =>
      FiberRef
        .make(LogContext.empty)
        .map { ref =>
          LoggerWithFormat(ref, appender.get)
        }
    )

  def modifyLogger(
    fn: InstrumentationLogger => InstrumentationLogger
  ): ZLayer[InstrumentationLogging, Nothing, InstrumentationLogging] =
    ZLayer.fromFunction[InstrumentationLogging, InstrumentationLogger](logging => fn(logging.get))

  def modifyLoggerM[R, E](
    fn: InstrumentationLogger => ZIO[R, E, InstrumentationLogger]
  ): ZLayer[InstrumentationLogging with R, E, InstrumentationLogging] =
    ZLayer.fromFunctionM[InstrumentationLogging with R, E, InstrumentationLogger](logging =>
      fn(logging.get).provide(logging)
    )

  def throwable(event: => InstrumentationEvent, t: Throwable): ZIO[InstrumentationLogging, Nothing, Unit] =
    ZIO.accessM[InstrumentationLogging](_.get.throwable(event, t))

  def trace(event: => InstrumentationEvent): ZIO[InstrumentationLogging, Nothing, Unit] =
    ZIO.accessM[InstrumentationLogging](_.get.trace(event))

  def warn(event: => InstrumentationEvent): ZIO[InstrumentationLogging, Nothing, Unit] =
    ZIO.accessM[InstrumentationLogging](_.get.warn(event))

  /**
   * Adds root logger name
   */
  def withRootLoggerName(name: String): ZLayer[InstrumentationLogging, Nothing, InstrumentationLogging] =
    modifyLogger(_.named(name))

  /**
   * modify initial context
   */
  def withContext(context: LogContext): ZLayer[InstrumentationLogging, Nothing, InstrumentationLogging] =
    modifyLogger(_.derive(_ => context))
}
