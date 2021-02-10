package morphir.flowz.instrumentation

import morphir.flowz.{ NodePath, StepExecutionId }
import zio.Cause
import zio.logging.LogFormat.LineFormatter
import zio.logging.{ LogContext, LogFormat }

/**
 * An event used to instrument (provide information) about the execution of a flow, process, step, behavior, etc.
 */
sealed abstract class InstrumentationEvent
object InstrumentationEvent {

  def logLine(line: String): LogLine = LogLine(line)
  def stepExecutionStarted(
    message: String,
    executionId: StepExecutionId,
    label: String,
    path: Option[NodePath] = None
  ): StepExecutionStarted =
    StepExecutionStarted(message, executionId, label, path)

  def stepExecutionStarted(uid: StepExecutionId, label: String, path: Option[NodePath]): StepExecutionStarted = {
    val message = s"Step execution started for Step[Label=$label; Uid=$uid;]"
    StepExecutionStarted(message, uid, label, path)
  }

  def stepExecutionStarted(uid: StepExecutionId, label: String): StepExecutionStarted = {
    val message = s"Step execution started for Step[Label=$label; Uid=$uid;]"
    StepExecutionStarted(message, uid, label, None)
  }

  def stepExecutionFailed(
    message: String,
    executionId: StepExecutionId,
    label: String,
    cause: Cause[Any] = Cause.empty,
    path: Option[NodePath] = None
  ): StepExecutionFailed = StepExecutionFailed(message, executionId, label, cause, path)

  def stepExecutionFailed(
    executionId: StepExecutionId,
    label: String,
    cause: Cause[Any]
  ): StepExecutionFailed =
    stepExecutionFailed(executionId, label, cause, None)

  def stepExecutionFailed(
    executionId: StepExecutionId,
    label: String,
    cause: Cause[Any],
    path: Option[NodePath]
  ): StepExecutionFailed = {
    val message = s"Step execution failed for Step[Label=$label; Uid=$executionId], because of ${cause.prettyPrint}"
    StepExecutionFailed(message, executionId, label, cause, path)
  }

  def stepExecutionSucceeded(
    message: String,
    executionId: StepExecutionId,
    label: String,
    path: Option[NodePath] = None
  ): StepExecutionSucceeded = StepExecutionSucceeded(message, executionId, label, path)

  def stepExecutionSucceeded(
    executionId: StepExecutionId,
    label: String
  ): StepExecutionSucceeded =
    stepExecutionSucceeded(executionId, label, None)

  def stepExecutionSucceeded(
    executionId: StepExecutionId,
    label: String,
    path: Option[NodePath]
  ): StepExecutionSucceeded = {
    val message = s"Step execution succeeded for Step[Label=$label; Uid=$executionId]"
    StepExecutionSucceeded(message, executionId, label, path)
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

  final case class StepExecutionStarted(
    message: String,
    executionId: StepExecutionId,
    label: String,
    path: Option[NodePath] = None
  ) extends InstrumentationEvent

  final case class StepExecutionFailed(
    message: String,
    executionId: StepExecutionId,
    label: String,
    cause: Cause[Any] = Cause.empty,
    path: Option[NodePath] = None
  ) extends InstrumentationEvent

  final case class StepExecutionSucceeded(
    message: String,
    executionId: StepExecutionId,
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
