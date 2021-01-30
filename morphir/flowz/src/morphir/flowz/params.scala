package morphir.flowz

import zio._

object params {
  type Params[A] = Has[ParamsRef[A]]

  /** Get the value contained in the Params. */
  def get[A: Tag]: URIO[Params[A], A] =
    ZIO.accessM(_.get.get)

  final case class ParamsRef[A](private val ref: Ref[A], tag: Tag[A]) { self =>
    implicit def getTag: Tag[A] = tag
    def get: UIO[A]             = ref.get
  }
}
