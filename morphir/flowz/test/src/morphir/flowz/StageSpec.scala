package morphir.flowz

import zio.test._
import zio.test.Assertion._
import zio.test.environment.TestSystem

object StageSpec extends DefaultRunnableSpec {
  def spec = suite("Stage Spec")(
    suite("Constructing")(
      testM("It should be possible to create a flow that always succeeds with the unit value")(
        for {
          output <- Act.unit.run
        } yield assert(output)(equalTo(StepOutputs.unit))
      ),
      testM("It should be possible to create a flow that always succeeds with None")(
        for {
          output <- Act.none.run
        } yield assert(output)(equalTo(StepOutputs.none))
      ),
      testM("It should be possible to create a flow that always succeeds with a value")(
        for {
          output <- Act.succeed(42).run
        } yield assert(output)(equalTo(StepOutputs.fromValue(42)))
      ),
      testM(
        "It should be possible to create a flow that always succeeds with a value (and it should pass-thru the state)"
      )(
        for {
          output <- Act.succeed(42).run("Hello", List("--help"))
        } yield assert(output)(equalTo(StepOutputs(value = 42, state = List("--help"))))
      ),
      testM("It should be possible to create a flow that always succeeds with the given output and state")(
        for {
          actual <- Act.succeedWith(value = 42, state = "What is the answer?").run
        } yield assert(actual)(equalTo(StepOutputs(state = "What is the answer?", value = 42)))
      ),
      testM("It should be possible to create a flow that always fails with a value")(
        for {
          result <- Act.fail("NO!!!").run.run
        } yield assert(result)(fails(equalTo("NO!!!")))
      ),
      testM("It should be possible to create a flow that produces the value of executing a function")(
        checkM(Gen.int(1, 5000)) { input =>
          for {
            actual  <- Act.fromFunction { n: Int => n * 2 }.run(input)
            expected = StepOutputs.assignBoth(input * 2)
          } yield assert(actual)(equalTo(expected))
        }
      ),
      testM("It should be possible to create a flow from an Option when the value is a Some")(
        for {
          actual <- Act.fromOption(Some(Widget("sprocket"))).run.map(_.value)
        } yield assert(actual)(equalTo(Widget("sprocket")))
      ),
      testM("It should be possible to create a flow from an Option when the value is a None")(
        for {
          actual <- Act.fromOption(None).run.map(_.value).run
        } yield assert(actual)(fails(isNone))
      )
    ),
    suite("Combining")(
      testM("It should be possible to combine steps using the >>> operator.") {
        val start   = Act.parameters[List[String]]
        val next    = Act.stateful((_: Any, args: List[String]) => (args, args.headOption))
        val myStage = start >>> next
        assertM(myStage.run(List("Hello", "World")))(
          equalTo(StepOutputs(state = List("Hello", "World"), value = Option("Hello")))
        )
      },
      testM("The parameters constructor should pass through the state it is given") {
        val givenState = List("A", "B", "C")
        val givenParam = "Hello"
        val sut        = Act.parameters[String]
        assertM(sut.run(givenParam, givenState))(
          equalTo(StepOutputs(state = givenState, value = givenParam))
        )
      },
      testM("It should be possible to combine flows using zip") {
        val flowA                                                           = Act.withStateAndValue("A")
        val flowB                                                           = Act.withStateAndValue(1)
        val flow: Act[Any, (String, Int), Any, Any, Nothing, (String, Int)] = flowA zip flowB
        assertM(flow.run)(equalTo(StepOutputs(state = ("A", 1), value = ("A", 1))))
      },
      testM("It should be possible to combine flows using the zip operator <*>") {
        val flowA                                                           = Act.withStateAndValue("A")
        val flowB                                                           = Act.withStateAndValue(1)
        val flow: Act[Any, (String, Int), Any, Any, Nothing, (String, Int)] = flowA <*> flowB
        assertM(flow.run)(equalTo(StepOutputs(state = ("A", 1), value = ("A", 1))))
      },
      testM("It should be possible to sequence flows using flatMap") {
        val flow = Act.succeed("true").flatMap(value => Act.succeed(s"The answer is: $value"))
        assertM(flow.run)(equalTo(StepOutputs.fromValue("The answer is: true")))
      },
      testM("It should be possible to sequence flows using a for comprehension") {
        for {
          _ <- TestSystem.putEnv("PROFILE", "local")
          out <- (for {
                   cfg             <- Act.succeed(Map("profile.local.host" -> "127.0.0.1", "profile.default.host" -> "finos.org"))
                   selectedProfile <- Act.fromEffect(zio.system.envOrElse("PROFILE", "default"))
                   host            <- Act.succeed(cfg.getOrElse(s"profile.$selectedProfile.host", "morphir.org"))
                 } yield host).run.map(_.value)
        } yield assert(out)(equalTo("127.0.0.1"))
      }
    ),
    testM("It should be possible to rename a step without affecting its value") {
      val theStage = Act.succeed("Good Boy!")
      val named    = Act.name("Praise")(theStage)

      for {
        original <- theStage.run
        actual   <- named.run
      } yield assert(actual)(equalTo(original))
    },
    testM("It should be possible to rename a step without affecting its value") {
      val theStage = Act.succeed("Good Boy!")
      val named    = Act.name("Praise")(theStage)

      for {
        original <- theStage.run
        actual   <- named.run
      } yield assert(actual)(equalTo(original))
    }
  )

  final case class Widget(name: String)
}
