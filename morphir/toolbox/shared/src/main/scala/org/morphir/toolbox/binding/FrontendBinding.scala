package org.morphir.toolbox.binding
import zio.stream._
import org.morphir.toolbox.binding.FrontendBinding.{InputPort, OutputPort}

case class FrontendBinding[S](
    input: InputPort[Any],
    output: OutputPort[Any],
    initialState: S
)

object FrontendBinding {

  type InputPort[+A] = Stream[Nothing, FrontendCommand[A]]

  type OutputPort[+A] = Stream[Nothing, FrontendEvent[A]]

  sealed trait FrontendCommand[+A]
  sealed trait FrontendEvent[+A]
}
