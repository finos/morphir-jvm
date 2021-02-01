package morphir.flowz

import zio._

import java.time.Instant

object trigger {
  type Trigger[Msg] = Has[TriggeringMessage[Msg]]

  def message[Msg: Tag]: URIO[Trigger[Msg], Msg] =
    ZIO.access(_.get.message)

  def timestamp[Msg: Tag]: URIO[Trigger[Msg], Instant] =
    ZIO.access(_.get.timestamp)

  final case class TriggeringMessage[Msg](message: Msg, timestamp: Instant)
  object TriggeringMessage {
    implicit def triggerInfoFromMessage[Payload](message: Message[Payload]): TriggeringMessage[Payload] =
      TriggeringMessage(message = message.payload, timestamp = message.timestamp)
  }
}
