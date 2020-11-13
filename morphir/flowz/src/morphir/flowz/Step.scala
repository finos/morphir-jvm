package morphir.flowz

import zio._

final case class Step[-R, -P, +E, +A](private val effect: ZIO[(R, P), E, A]) { self =>

  def adaptParameters[P0](func: P0 => P): Step[R, P0, E, A] =
    Step(effect.provideSome { case (r, p0) =>
      (r, func(p0))
    })

  def map[A1](func: A => A1): Step[R, P, E, A1] =
    Step(self.effect.map(func))

  def run(implicit ev: Any <:< P): ZIO[R, E, A] =
    self.effect.provideSome[R](r => (r, ()))

  def run(params: P): ZIO[R, E, A] =
    self.effect.provideSome[R](r => (r, params))
}

object Step {

  /**
   * Creates a `Step` that always fails with an error of type `E`.
   */
  def fail[E](error: => E): Step[Any, Any, E, Nothing] =
    Step(ZIO.fail(error))

  def fromFunction[P, A](func: P => A): Step[Any, P, Throwable, A] =
    Step(ZIO.environment[(Any, P)].mapEffect { case (_, p) => func(p) })

  def stepM[R, P, E, A](func: P => ZIO[R, E, A]): Step[R, P, E, A] =
    Step(ZIO.environment[(R, P)].flatMap { case (r, p) =>
      func(p).provide(r)
    })

  def succeed[A](value: => A): Step[Any, Any, Nothing, A] =
    Step(ZIO.succeed(value))

}
