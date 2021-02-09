package morphir.flowz.experimental

import zio._

object output {
  type Output[A] = Has[OutputRef[A]]

  /** Get the value contained in the Output. */
  def value[A: Tag]: URIO[Output[A], A] =
    ZIO.accessM(_.get.value)

  final case class OutputRef[A](private val ref: Ref[A], tag: Tag[A]) { self =>
    implicit def getTag: Tag[A] = tag
    def value: UIO[A]           = ref.get

  }
}
