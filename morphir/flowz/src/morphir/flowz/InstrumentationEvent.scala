package morphir.flowz

import zio.logging.LogFormat.LineFormatter
import zio.logging.{ LogContext, LogFormat }

/**
 * An event used to instrument (provide information) about the execution of a flow, process, step, behavior, etc.
 */
sealed abstract class InstrumentationEvent
object InstrumentationEvent {

  def logLine(line: String): LogLine = LogLine(line)

  /**
   * Models an informational message. The informational message includes the message text, contextData,
   * a source and the possible path to the node in the flow graph which
   * was being executed (i.e. the step where this failure occurred).
   */
  final case class Information[+Data](message: String, contextData: Data, source: String, path: Option[NodePath] = None)
      extends InstrumentationEvent

  /**
   * Models a warning. The warning includes a message, contextData, a source and the possible path to the node
   * in the flow graph which was being executed (i.e. the step where this failure occurred).
   */
  final case class Warning[+Data](message: String, contextData: Data, source: String, path: Option[NodePath] = None)
      extends InstrumentationEvent

  /**
   * Models a trace message, which is often used for developer and diagnostic purposes.
   * The trace message includes the message text, contextData,
   * a source and the possible path to the node in the flow graph which
   * was being executed (i.e. the step where this failure occurred).
   */
  final case class Trace[+Data](message: String, contextData: Data, source: String, path: Option[NodePath] = None)
      extends InstrumentationEvent

  /**
   * A raw context free instrumentation event for logging a line of text.
   */
  final case class LogLine(line: String) extends InstrumentationEvent

  /**
   * Contains various log formats for `InstrumentationEvent`s.
   */
  object logFormats {

    /**
     * A log format for logging instrumentation events to the console with colored messages.
     */
    val coloredConsoleLogFormat: LogFormat[InstrumentationEvent] = new LogFormat[InstrumentationEvent] {
      val lineFormatter: LineFormatter = (_, s) => s
      private val underlyingLogFormat  = LogFormat.ColoredLogFormat(lineFormatter)

      def format(context: LogContext, line: InstrumentationEvent): String =
        underlyingLogFormat.format(context, eventToLogLine(line))

      def eventToLogLine(event: InstrumentationEvent): String =
        //TODO: Do some appropriate formatting here
        event.toString
    }

    /**
     * A log format for logging instrumentation events to the console.
     */
    val simpleConsoleLogFormat: LogFormat[InstrumentationEvent] = new LogFormat[InstrumentationEvent] {
      val lineFormatter: LineFormatter = (_, s) => s
      private val underlyingLogFormat  = LogFormat.SimpleConsoleLogFormat(lineFormatter)

      def format(context: LogContext, line: InstrumentationEvent): String =
        underlyingLogFormat.format(context, eventToLogLine(line))

      def eventToLogLine(event: InstrumentationEvent): String =
        //TODO: Do some appropriate formatting here
        event.toString
    }
  }
}
