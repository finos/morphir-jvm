package morphir.flowz
import morphir.flowz.instrumentation.{ InstrumentationEvent, InstrumentationLogging }
import zio.clock.Clock
import zio.console.Console
import zio.logging.{ LogFormat, LogLevel, Logger }
import zio.{ Has, UIO, URIO, ZIO, ZLayer }

object instrumentor {
  type Instrumentor = Has[Instrumentor.Service]

  def logLine(line: String): URIO[Instrumentor, Unit] =
    ZIO.accessM(_.get.logLine(line))

  object Instrumentor {
    trait Service {
      def logLine(line: String): UIO[Unit]

    }

    final case class LoggingInstrumentor(logger: Logger[InstrumentationEvent]) extends Service {
      def logLine(line: String): UIO[Unit] = logger.log(InstrumentationEvent.logLine(line))
    }

    val fromLogger: ZLayer[InstrumentationLogging, Nothing, Instrumentor] = ZLayer.fromService {
      logger: Logger[InstrumentationEvent] => LoggingInstrumentor(logger)
    }

    def console(
      logLevel: LogLevel = LogLevel.Info,
      format: LogFormat[InstrumentationEvent] = InstrumentationEvent.logFormats.coloredConsoleLogFormat
    ): ZLayer[Console with Clock, Nothing, Instrumentor] =
      InstrumentationLogging.console(logLevel, format).map(l => Has(LoggingInstrumentor(l.get)))
  }
}
