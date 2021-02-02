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
        } yield assert(result)(equalTo(BehaviorSuccess(21, 42)))
      ),
      testM("It should be possible to construct a Behavior that always fails with a given value")(
        for {
          result <- Behavior.fail("Oops!").run.run
        } yield assert(result)(fails(equalTo("Oops!")))
      ),
      testM("It should be possible to construct a Behavior that modifies its output given an initial state")(
        for {
          result <- Behavior.modify { text: String => s"$text:${text.size}" -> text.size }.run("Hello", ())
        } yield assert(result)(equalTo(BehaviorSuccess("Hello:5", 5)))
      ),
      testM("It should be possible to construct a Behavior from a simple update function")(
        for {
          result <-
            Behavior
              .update[List[String], List[String], String, String] { case (initialState: List[String], msg: String) =>
                (msg :: initialState, msg.reverse)
              }
              .run(List("John", "Joe"), "Jack")
        } yield assert(result)(equalTo(BehaviorSuccess(state = List("Jack", "John", "Joe"), result = "kcaJ")))
      ),
      testM("It should be possible to construct a behavior that gets the initial state unchanged.")(
        for {
          result <- Behavior.get[Set[Int]].run(Set(1, 2, 3, 4), Set(5, 6, 7, 8))
        } yield assert(result)(equalTo(BehaviorSuccess(Set(1, 2, 3, 4), Set(1, 2, 3, 4))))
      ),
      testM("It should be possible to construct a behavior that sets the state to a value.")(
        checkM(Gen.alphaNumericString, Gen.alphaNumericString) { (input, s1) =>
          for {
            result <- Behavior.set(input).run(s1, "Something")
          } yield assert(result)(equalTo(BehaviorSuccess(state = input, result = ())))
        }
      )
    ),
    suite("Operations")(
      testM("It should be possible to return a different constant value using as")(
        for {
          result <- Behavior.unit.as("Foo").run("S1", ())
        } yield assert(result)(equalTo(BehaviorSuccess("S1", "Foo")))
      )
    ),
    suite("Combining")(
      testM("It should be possible to sequence flows using flatMap") {
        val behavior = Behavior.succeed("true").flatMap(value => Behavior.succeed(s"The answer is: $value")).run(21, 21)
        assertM(behavior)(equalTo(BehaviorSuccess(21, "The answer is: true")))
      }
    )
  )
}
