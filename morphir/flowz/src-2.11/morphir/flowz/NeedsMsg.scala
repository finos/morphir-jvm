package morphir.flowz

import scala.annotation.implicitNotFound

/**
 * A value of type `NeedsMsg[Msg]` provides implicit evidence that an effect with
 * error type `Msg` can fail, that is, that `Msg` is not equal to `Nothing`.
 */
@implicitNotFound(
  "This operation assumes that your behavior, flow, step, or effect requires a message to be provided as input. " +
    "However, your behavior has Any for the message type, which means it " +
    "has no requirement, so there is no need to provide the message."
)
sealed abstract class NeedsMsg[+Msg]

object NeedsMsg extends NeedsMsg[Nothing] {

  implicit def needsMsg[S]: NeedsMsg[S] = NeedsMsg

  // Provide multiple ambiguous values so an implicit NeedsNsg[Nothing] cannot be found.
  implicit val needMsgAmbiguous1: NeedsMsg[Any]  = NeedsMsg
  implicit val needsMsgAmbiguous2: NeedsMsg[Any] = NeedsMsg
}
