package morphir.flowz
import scala.annotation.implicitAmbiguous

/**
 * A value of type `NeedsMsg[S]` provides implicit evidence that a behavior with
 * error type `E` can fail, that is, that `E` is not equal to `Nothing`.
 */
sealed abstract class NeedsMsg[-Msg]

object NeedsMsg extends NeedsMsg[Any] {

  implicit def needsMsg[Msg]: NeedsMsg[Msg] = NeedsMsg

  // Provide multiple ambiguous values so an implicit NeedsInputState[Nothing] cannot be found.
  // Pro  vide multiple ambiguous values so an implicit CanFail[Nothing] cannot be found.
  @implicitAmbiguous(
    "This error handling operation assumes your effect can fail. However, " +
      "your effect has Nothing for the error type, which means it cannot " +
      "fail, so there is no need to handle the failure. To find out which " +
      "method you can use instead of this operation, please see the " +
      "reference chart at: https://zio.dev/docs/can_fail"
  )
  implicit val needsMsgAmbiguous1: NeedsMsg[Nothing] = NeedsMsg
  implicit val needsMsgAmbiguous2: NeedsMsg[Nothing] = NeedsMsg
}
