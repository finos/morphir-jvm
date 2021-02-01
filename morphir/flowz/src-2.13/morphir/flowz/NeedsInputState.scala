package morphir.flowz

import scala.annotation.implicitNotFound

/**
 * A value of type `CanFail[E]` provides implicit evidence that an effect with
 * error type `E` can fail, that is, that `E` is not equal to `Nothing`.
 */
@implicitNotFound(
  "This operation assumes that your behavior (or effect, ste, or flow) requires an input state. " +
    "However, your behavior has Any for the environment type, which means it " +
    "has no requirement, so there is no need to provide the input state."
)
sealed abstract class NeedsInputState[-S]

object NeedsInputState extends NeedsInputState[Any] {

  implicit def needsInputState[S]: NeedsInputState[S] = NeedsInputState

  // Provide multiple ambiguous values so an implicit NeedsInputState[Nothing] cannot be found.
  implicit val needsInputStateAmbiguous1: NeedsInputState[Nothing] = NeedsInputState
  implicit val needsInputStateAmbiguous2: NeedsInputState[Nothing] = NeedsInputState
}
