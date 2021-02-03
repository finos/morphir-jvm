package morphir.flowz

import zio.Cause

sealed abstract class BehaviorFailure[+E]
object BehaviorFailure {
  final case class Runtime[+E](cause: Cause[E]) extends BehaviorFailure[E]
}
