package zio.morphir.sdk
import zio.prelude.*

object ResultModule {
  type Result[+E, +A] = Validation[E, A]
  object Result {

    def ok[A](value: A): Result[Nothing, A] = Validation.succeed(value)

    def Ok[A](value: A): Result[Nothing, A] = Validation.succeed(value)

    def Err[E](error: E): Result[E, Nothing] = Validation.fail(error)
    def err[E](error: E): Result[E, Nothing] = Validation.fail(error)

    def map[E, A](result: Result[E, A])(f: A => A): Result[E, A] = result.map(f)
  }
}
