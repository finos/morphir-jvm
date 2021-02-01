package morphir.flowz

import scala.annotation.implicitAmbiguous

/**
 * A value of type `NeedsInputState[E]` provides implicit evidence that a behavior with
 * error type `E` can fail, that is, that `E` is not equal to `Nothing`.
 */
sealed abstract class NeedsInputState[-S]

object NeedsInputState extends NeedsInputState[Any] {

  implicit def needsInputState[S]: NeedsInputState[S] = NeedsInputState

  // Provide multiple ambiguous values so an implicit NeedsInputState[Nothing] cannot be found.
  // Provide multiple ambiguous values so an implicit CanFail[Nothing] cannot be found.
  @implicitAmbiguous(
    "This error handling operation assumes your effect can fail. However, " +
      "your effect has Nothing for the error type, which means it cannot " +
      "fail, so there is no need to handle the failure. To find out which " +
      "method you can use instead of this operation, please see the " +
      "reference chart at: https://zio.dev/docs/can_fail"
  )
  implicit val needsInputStateAmbiguous1: NeedsInputState[Nothing] = NeedsInputState
  implicit val needsInputStateAmbiguous2: NeedsInputState[Nothing] = NeedsInputState
}
