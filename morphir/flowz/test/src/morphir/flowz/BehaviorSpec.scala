package morphir.flowz

import zio.test._
import zio.test.Assertion._

object BehaviorSpec extends DefaultRunnableSpec {
  def spec = suite("Behavior Spec")(
    suite("When constructing a Behavior")(
      testM("It should be possible to construct a Behavior that always succeeds with a given value.")(
        for {
          result <- Behavior.succeed(42).runResult
        } yield assert(result)(equalTo(42))
      ),
      testM(
        "It should be possible to construct a Behavior that always succeeds with a given value and honors the passed in state."
      )(
        for {
          result <- Behavior.succeed(42).run(21, ())
        } yield assert(result)(equalTo(BehaviorResult(21, 42)))
      ),
      testM("It should be possible to construct a Behavior that always fails with a given value")(
        for {
          result <- Behavior.fail("Oops!").run.run
        } yield assert(result)(fails(equalTo("Oops!")))
      ),
      testM("It should be possible to construct a Behavior that modifies its output given an initial state")(
        for {
          result <- Behavior.modify { text: String => s"$text:${text.size}" -> text.size }.run("Hello", ())
        } yield assert(result)(equalTo(BehaviorResult("Hello:5", 5)))
      ),
      testM("It should be possible to construct a Behavior from a simple update function")(
        for {
          result <-
            Behavior
              .update[List[String], List[String], String, String] { case (initialState: List[String], msg: String) =>
                (msg :: initialState, msg.reverse)
              }
              .run(List("John", "Joe"), "Jack")
        } yield assert(result)(equalTo(BehaviorResult(state = List("Jack", "John", "Joe"), result = "kcaJ")))
      )
    )
  )
}
