package morphir.flowz.experimental

import zio._

final case class Step[-R, -P, +E, +A](private val effect: ZIO[(R, P), E, A]) { self =>

  def >>>[R1 <: R, E1 >: E, A1](that: Step[R1, A, E1, A1]): Step[R1, P, E1, A1] =
    self andThen that

  def *>[R1 <: R, P1 <: P, E1 >: E, A1](that: Step[R1, P1, E1, A1]): Step[R1, P1, E1, A1] =
    Step(ZIO.environment[(R1, P1)].flatMap { case (r, p) =>
      self.effect *> that.effect.provide((r, p))
    })

  def <*>[R1 <: R, P1 <: P, E1 >: E, B](that: Step[R1, P1, E1, B]): Step[R1, P1, E1, (A, B)] =
    self zip that

  /**
   * An operator alias for `zipPar`
   */
  def <&>[R1 <: R, P1 <: P, E1 >: E, B](that: Step[R1, P1, E1, B]): Step[R1, P1, E1, (A, B)] =
    self zipPar that

  /**
   * An operator alias for `zipPar`
   */
  def |+|[R1 <: R, P1 <: P, E1 >: E, B](that: Step[R1, P1, E1, B]): Step[R1, P1, E1, (A, B)] =
    self zipPar that

  def ++[R1 <: R, E1 >: E, P2, B](that: Step[R1, P2, E1, B]): Step[R1, (P, P2), E1, (A, B)] =
    Step(ZIO.environment[(R1, (P, P2))].flatMap { case (r, (p, p2)) =>
      self.effect.provide((r, p)) zip that.effect.provide((r, p2))
    })

  def adaptInput[P0](func: P0 => P): Step[R, P0, E, A] =
    adaptParameters[P0](func)

  def adaptParameters[P0](func: P0 => P): Step[R, P0, E, A] =
    Step(effect.provideSome { case (r, p0) =>
      (r, func(p0))
    })

  def andThen[R1 <: R, E1 >: E, A1](that: Step[R1, A, E1, A1]): Step[R1, P, E1, A1] =
    Step(ZIO.environment[(R1, P)].flatMap { case (r, _) =>
      self.effect.flatMap(a => that.effect.provide((r, a)))
    })

  def compose[P1 <: P, E1 >: E, B](that: Step[A, P1, E1, B]): Step[R, P1, E1, B] =
    Step(
      for {
        inputs <- ZIO.environment[(R, P1)]
        r       = inputs._1
        p       = inputs._2
        a      <- self.effect.provide((r, p))
        b      <- that.effect.provide((a, p))
      } yield b
    )

  def flatMap[R1 <: R, P1 <: P, E1 >: E, A1](f: A => Step[R1, P1, E1, A1]): Step[R1, P1, E1, A1] =
    Step(self.effect.flatMap(a => f(a).effect))

  def flipInputs: Step[P, R, E, A] =
    Step(ZIO.environment[(P, R)].flatMap { case (p, r) =>
      self.effect.provide((r, p))
    })

  def map[A1](func: A => A1): Step[R, P, E, A1] =
    Step(self.effect.map(func))

  def mapEffect[A1](f: A => A1)(implicit ev: E <:< Throwable): Step[R, P, Throwable, A1] =
    Step(self.effect.mapEffect(f))

  def mapError[E1](func: E => E1)(implicit ev: CanFail[E]): Step[R, P, E1, A] =
    Step(self.effect.mapError(func))

  def memoize: Step[Any, Any, Nothing, ZIO[(R, P), E, A]] =
    Step(self.effect.memoize)

  def provide(r: R): Step[Any, P, E, A] =
    Step(ZIO.environment[(Any, P)].flatMap { case (_, p) =>
      self.effect.provide((r, p))
    })

  def provideParameters(params: P): Step[R, Any, E, A] =
    Step(ZIO.environment[(R, Any)].flatMap { case (r, _) =>
      self.effect.provide((r, params))
    })

  def run(implicit ev: Any <:< P): ZIO[R, E, A] =
    self.effect.provideSome[R](r => (r, ()))

  def run(params: P): ZIO[R, E, A] =
    self.effect.provideSome[R](r => (r, params))

  def transform[R1 <: R, P1 <: P, B](func: (R1, P1, A) => B): Step[R1, P1, E, B] =
    Step(ZIO.environment[(R1, P1)].flatMap { case (r, p) =>
      self.effect.map(a => func(r, p, a))
    })

  def zip[R1 <: R, P1 <: P, E1 >: E, B](that: Step[R1, P1, E1, B]): Step[R1, P1, E1, (A, B)] =
    Step(self.effect zip that.effect)

  def zipPar[R1 <: R, P1 <: P, E1 >: E, B](that: Step[R1, P1, E1, B]): Step[R1, P1, E1, (A, B)] =
    Step(self.effect zipPar that.effect)

  def zipWith[R1 <: R, P1 <: P, E1 >: E, B, C](that: Step[R1, P1, E1, B])(
    combine: (A, B) => C
  ): Step[R1, P1, E1, C] =
    Step(self.effect.zipWith(that.effect)(combine))

  def zipWithPar[R1 <: R, P1 <: P, E1 >: E, B, C](that: Step[R1, P1, E1, B])(
    combine: (A, B) => C
  ): Step[R1, P1, E1, C] =
    Step(self.effect.zipWithPar(that.effect)(combine))
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
