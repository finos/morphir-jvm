package morphir.toolbox.binding

import zio.stream._
import morphir.toolbox.binding.FrontendBinding.{ FrontendCommand, FrontendEvent, InputPort, OutputPort }
import morphir.toolbox.core.ProjectInfo

case class FrontendBinding[S](
  name: String,
  input: InputPort[FrontendCommand[Any]],
  output: OutputPort[FrontendEvent[Any]],
  initialState: S
)

object FrontendBinding {

  type InputPort[+A] = Stream[Nothing, FrontendCommand[A]]

  type OutputPort[+A] = Stream[Nothing, FrontendEvent[A]]

  sealed trait FrontendCommand[+A]
  object FrontendCommand {
    final case class ConnectProject(project: ProjectInfo) extends FrontendCommand[Nothing]
  }

  sealed trait FrontendEvent[+A]
  object FrontendEvent {}
}
