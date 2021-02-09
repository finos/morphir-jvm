package morphir.flowz

import zio.Cause

sealed abstract class StepFailure[+E]
object StepFailure {
  final case class Runtime[+E](cause: Cause[E]) extends StepFailure[E]
}
