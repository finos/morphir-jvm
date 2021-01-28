package morphir.flowz

object StepVisibility {
  case object Visible extends StepVisibility
  case object Hidden  extends StepVisibility

  val value: Set[StepVisibility] = Set(Visible, Hidden)
}

/**
 * The visibility of a Step.
 */
sealed abstract class StepVisibility extends Product with Serializable
