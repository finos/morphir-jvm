package morphir.flowz

import zio._

object inputState {
  type InputState[A] = Has[StateIn[A]]

  /** Get the value contained in the Input. */
  def getValue[A: Tag]: URIO[InputState[A], A] =
    ZIO.access(_.get.value)

  final case class StateIn[A](value: A, tag: Tag[A]) { self =>
    implicit def getTag: Tag[A] = tag
    def getValue: UIO[A]        = UIO.succeed(value)
    def toInput: InputState[A]  = Has(self)
  }
}
