package morphir.flowz

import zio.Cause

sealed abstract class FlowFailure[+E]
object FlowFailure {
  //TODO: Add in Precondition, PostCondition, and Invariant as possible other failure types
  //      these can be built directly ontop of Assertion or using zio-prelude's Validation

  final case class Runtime[+E](cause: Cause[E]) extends FlowFailure[E]

  def die(t: Throwable): FlowFailure[Nothing] =
    halt(Cause.die(t))

  def fail[E](e: E): FlowFailure[E] =
    halt(Cause.fail(e))

  def halt[E](cause: Cause[E]): FlowFailure[E] =
    Runtime(cause)
}
