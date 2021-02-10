package morphir.flowz.platform
import zio.ZIO
import zio.stream._

final case class Cmd[-R, +E, +Msg] private (messages: ZStream[R, E, Msg]) { self =>
  def ++[R1 <: R, E1 >: E, Msg1 >: Msg](that: Cmd[R1, E1, Msg1]): Cmd[R1, E1, Msg1] =
    Cmd(self.messages ++ that.messages)
}

object Cmd {
  def batch[R, E, Msg](commands: List[Cmd[R, E, Msg]]): Cmd[R, E, Msg] =
    Cmd(ZStream.fromIterable(commands).flatMap(cmd => cmd.messages))

  val none: Cmd[Any, Nothing, Nothing] =
    Cmd(ZStream.empty)

  def ofMsg[R, E, Msg](effect: ZIO[R, E, Msg]): Cmd[R, E, Msg] =
    Cmd(ZStream.fromEffect(effect))

  def ofMsg[Msg](effect: => Msg): Cmd[Any, Throwable, Msg] =
    Cmd(ZStream.fromEffect(ZIO.effect(effect)))

  def ofMsgTotal[Msg](effect: => Msg): Cmd[Any, Nothing, Msg] =
    Cmd(ZStream.succeed(effect))

  val unit: Cmd[Any, Nothing, Unit] =
    Cmd(ZStream.unit)
}
