package morphir.flowz.platform
import zio.stream._

final case class Cmd[-R, +E, +Msg](messages: ZStream[R, E, Msg])

object Cmd {
  val none: Cmd[Any, Nothing, Nothing] =
    Cmd(ZStream.empty)

  val unit: Cmd[Any, Nothing, Unit] =
    Cmd(ZStream.unit)
}
