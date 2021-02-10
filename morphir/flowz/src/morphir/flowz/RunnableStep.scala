package morphir.flowz
import morphir.flowz.instrumentation.InstrumentationEvent
import zio.ZIO

final case class RunnableStep[-SIn, +SOut, -Msg, -R, +E, +A](
  override val label: Option[String],
  underlyingStep: Step[SIn, SOut, Msg, R, E, A]
) extends Step[SIn, SOut, Msg, R with StepRuntimeEnv, E, A] {

  /**
   * Defines the underlying behavior of this `Step`.
   */
  protected[flowz] def behavior(state: SIn, message: Msg): ZIO[R with StepRuntimeEnv, E, StepSuccess[SOut, A]] =
    for {
      uid           <- StepUid.nextUid
      labelResolved <- ZIO.succeed(label getOrElse "N/A")
      _             <- iLog.trace(InstrumentationEvent.stepExecutionStarted(uid, labelResolved))
      result <-
        underlyingStep
          .behavior(state, message)
          .tapCause(cause => iLog.error(InstrumentationEvent.stepExecutionFailed(uid, labelResolved, cause), cause))
          .tap(_ => iLog.trace(InstrumentationEvent.stepExecutionSucceeded(uid, labelResolved)))
    } yield result

  /**
   * Runs the step.
   */
  final def run(implicit ev1: Any <:< SIn, ev2: Any <:< Msg): ZIO[R with StepRuntimeEnv, E, StepSuccess[SOut, A]] =
    run((), ())

  /**
   * Runs the step.
   */
  final def run(state: SIn, message: Msg): ZIO[R with StepRuntimeEnv, E, StepSuccess[SOut, A]] =
    behavior(state, message)

  /**
   * Runs the step.
   */
  final def run(message: Msg)(implicit ev: Any <:< SIn): ZIO[R with StepRuntimeEnv, E, StepSuccess[SOut, A]] =
    run((), message)

  /**
   * Runs the step.
   */
  final def runResult(implicit ev1: Any <:< SIn, ev2: Any <:< Msg): ZIO[R with StepRuntimeEnv, E, A] =
    run((), ()).map(_.result)
}

object RunnableStep {
  def step[SIn, SOut, Msg, R, Err, A](
    label: String
  )(theStep: Step[SIn, SOut, Msg, R, Err, A]): RunnableStep[SIn, SOut, Msg, R, Err, A] =
    theStep match {
      case runnable @ RunnableStep(_, _) => runnable.asInstanceOf[RunnableStep[SIn, SOut, Msg, R, Err, A]]
      case _                             => RunnableStep(Option(label), theStep)
    }
}
