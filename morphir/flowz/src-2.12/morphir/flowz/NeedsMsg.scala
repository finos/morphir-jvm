package morphir.flowz
import scala.annotation.implicitAmbiguous

/**
 * A value of type `NeedsMsg[Msg]` provides implicit evidence that a behavior with
 * message type `Msg` needs a message, that is, that `Msg` is not equal to `Any`.
 */
sealed abstract class NeedsMsg[+Msg]

object NeedsMsg extends NeedsMsg[Nothing] {

  implicit def needsMsg[Msg]: NeedsMsg[Msg] = NeedsMsg

  // Provide multiple ambiguous values so an implicit NeedsMsg[Nothing] cannot be found.
  @implicitAmbiguous(
    "This operation assumes that your behavior, flow, step, or effect requires a message to be provided as input. " +
      "However, your behavior has Any for the message type, which means it " +
      "has no requirement, so there is no need to provide the message."
  )
  implicit val needsMsgAmbiguous1: NeedsMsg[Any] = NeedsMsg
  implicit val needsMsgAmbiguous2: NeedsMsg[Any] = NeedsMsg
}
