package morphir.flowz

import scala.annotation.implicitAmbiguous

/**
 * A value of type `NeedsInputState[R]` provides implicit evidence that a behavior, flow, step, or effect with
 * input state type `S` needs a state value, that is, that `S` is not equal to
 * `Any`.
 */
sealed abstract class NeedsInputState[+S]

object NeedsInputState extends NeedsInputState[Nothing] {

  implicit def needsInputState[S]: NeedsInputState[S] = NeedsInputState

  // Provide multiple ambiguous values so an implicit NeedsInputState[Any] cannot be found.
  @implicitAmbiguous(
    "This operation assumes that your behavior, flow, step, or effect requires an input state. " +
      "However, your behavior has Any for the input state type, which means it " +
      "has no requirement, so there is no need to provide the input state."
  )
  implicit val needsInputStateAmbiguous1: NeedsInputState[Any] = NeedsInputState
  implicit val needsInputStateAmbiguous2: NeedsInputState[Any] = NeedsInputState
}
