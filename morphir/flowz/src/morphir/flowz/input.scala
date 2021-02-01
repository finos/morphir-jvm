package morphir.flowz

import zio._

object input {
  type Input[A] = Has[In[A]]

  /** Get the value contained in the Input. */
  def getValue[A: Tag]: URIO[Input[A], A] =
    ZIO.access(_.get.value)

  final case class In[A](value: A, tag: Tag[A]) { self =>
    implicit def getTag: Tag[A] = tag
    def getValue: UIO[A]        = UIO.succeed(value)
    def toInput: Input[A]       = Has(self)
  }
}
