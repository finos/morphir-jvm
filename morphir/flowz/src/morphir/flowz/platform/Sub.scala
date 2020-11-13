package morphir.flowz.platform
import zio.stream._
final case class Sub[-R, +E, +Msg](messages: ZStream[R, E, Msg])
object Sub {
  val none: Sub[Any, Nothing, Nothing] =
    Sub(ZStream.empty)
}
