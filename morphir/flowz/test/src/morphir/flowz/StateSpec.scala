package morphir.flowz

import zio.test.Assertion._
import zio.test._
import zio.ZIO

object StateSpec extends DefaultRunnableSpec {
  def spec = suite("State Spec")(
    suite("Given a brand new State instance")(
      testM("When you call get you should get the initial value")(
        for {
          initial <- ZIO.succeed("Hello")
          sut     <- state.make(initial)
          actual  <- state.get[String].provide(sut)
        } yield assert(actual)(equalTo(initial))
      )
    )
  )
}
