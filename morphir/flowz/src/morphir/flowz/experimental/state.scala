package morphir.flowz.experimental

import zio.{ Has, Ref, Tag, UIO, ULayer, URIO, ZIO }

object state {
  type State[A] = Has[StateRef[A]]

  /** Get the value contained in the State. */
  def get[A: Tag]: URIO[State[A], A] =
    ZIO.accessM(_.get.get)

  /** Make a layer from the given value */
  def makeLayer[A: Tag](initialValue: A): ULayer[State[A]] =
    StateRef.make(initialValue).toLayer

  /** Make an effect that creates a state */
  def make[A: Tag](initialValue: A): UIO[State[A]] =
    StateRef.make(initialValue).map(ref => Has(ref))

  final case class StateRef[A](private val ref: Ref[A], tag: Tag[A]) { self =>
    implicit def getTag: Tag[A] = tag
    @inline def get: UIO[A]     = ref.get

    def toState: State[A] = Has(self)
  }

  object StateRef {
    def make[A: Tag](initialValue: A): UIO[StateRef[A]] =
      Ref.make(initialValue).map(ref => StateRef(ref, Tag[A]))
  }

}
