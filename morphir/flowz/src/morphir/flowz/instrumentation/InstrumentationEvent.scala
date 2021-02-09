package morphir.flowz.instrumentation

import morphir.flowz.{ NodePath, StepUid }
import zio.Cause
import zio.logging.LogFormat.LineFormatter
import zio.logging.{ LogContext, LogFormat }

/**
 * An event used to instrument (provide information) about the execution of a flow, process, step, behavior, etc.
 */
sealed abstract class InstrumentationEvent
object InstrumentationEvent {

  def logLine(line: String): LogLine = LogLine(line)
  def runningStep(message: String, uid: StepUid, label: String, path: Option[NodePath] = None): RunningStep =
    RunningStep(message, uid, label, path)

  def stepExecutionFailed(
    message: String,
    uid: StepUid,
    label: String,
    cause: Cause[Any] = Cause.empty,
    path: Option[NodePath] = None
  ): StepExecutionFailed = StepExecutionFailed(message, uid, label, cause, path)

  def stepExecutionFailed(
    uid: StepUid,
    label: String,
    cause: Cause[Any]
  ): StepExecutionFailed =
    stepExecutionFailed(uid, label, cause, None)

  def stepExecutionFailed(
    uid: StepUid,
    label: String,
    cause: Cause[Any],
    path: Option[NodePath]
  ): StepExecutionFailed = {
    val message = s"Step execution failed for Step[Label=$label; Uid=$uid], because of ${cause.prettyPrint}"
    StepExecutionFailed(message, uid, label, cause, path)
  }

  def stepExecutionSucceeded(
    message: String,
    uid: StepUid,
    label: String,
    path: Option[NodePath] = None
  ): StepExecutionSucceeded = StepExecutionSucceeded(message, uid, label, path)

  def stepExecutionSucceeded(
    uid: StepUid,
    label: String
  ): StepExecutionSucceeded =
    stepExecutionSucceeded(uid, label, None)

  def stepExecutionSucceeded(
    uid: StepUid,
    label: String,
    path: Option[NodePath]
  ): StepExecutionSucceeded = {
    val message = s"Step execution succeeded for Step[Label=$label; Uid=$uid]"
    StepExecutionSucceeded(message, uid, label, path)
  }

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

  final case class RunningStep(message: String, uid: StepUid, label: String, path: Option[NodePath] = None)
      extends InstrumentationEvent

  final case class StepExecutionFailed(
    message: String,
    uid: StepUid,
    label: String,
    cause: Cause[Any] = Cause.empty,
    path: Option[NodePath] = None
  ) extends InstrumentationEvent

  final case class StepExecutionSucceeded(
    message: String,
    uid: StepUid,
    label: String,
    path: Option[NodePath] = None
  ) extends InstrumentationEvent

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
