package morphir.flowz

import zio.test._
import zio.test.Assertion._
import zio.test.environment.TestSystem
import morphir.flowz.default._

object DefaultFlowSpec extends DefaultRunnableSpec {
  def spec = suite("Flow Spec fro (default)")(
    suite("Constructing")(
      testM("It should be possible to create a flow that always succeeds with the unit value")(
        for {
          output <- Flow.unit.run
        } yield assert(output)(equalTo(OutputChannels.unit))
      ),
      testM("It should be possible to create a flow that always succeeds with None")(
        for {
          output <- Flow.none.run
        } yield assert(output)(equalTo(OutputChannels.none))
      ),
      testM("It should be possible to create a flow that always succeeds with a value")(
        for {
          output <- Flow.succeed(42).run
        } yield assert(output)(equalTo(OutputChannels.fromValue(42)))
      ),
      testM("It should be possible to create a flow that always succeeds with the given output and state")(
        for {
          actual <- Flow.succeed(value = 42, state = "What is the answer?").run
        } yield assert(actual)(equalTo(OutputChannels(state = "What is the answer?", value = 42)))
      ),
      testM("It should be possible to create a flow that always fails with a value")(
        for {
          result <- Flow.fail("NO!!!").run.run
        } yield assert(result)(fails(equalTo("NO!!!")))
      ),
      testM("It should be possible to create a flow that produces the value of executing a function")(
        checkM(Gen.int(1, 5000)) { input =>
          for {
            actual  <- Flow.fromFunction { n: Int => n * 2 }.run(input)
            expected = OutputChannels.fromValue(input * 2)
          } yield assert(actual)(equalTo(expected))
        }
      ),
      testM("It should be possible to create a flow from an Option when the value is a Some")(
        for {
          actual <- Flow.fromOption(Some(Widget("sprocket"))).run.map(_.value)
        } yield assert(actual)(equalTo(Widget("sprocket")))
      ),
      testM("It should be possible to create a flow from an Option when the value is a None")(
        for {
          actual <- Flow.fromOption(None).run.map(_.value).run
        } yield assert(actual)(fails(isNone))
      )
    ),
    suite("Combining")(
      testM("It should be possible to combine flows using the >>> operator.") {
        val start = Flow.parameters[List[String]]
        val next  = Flow((_: Any, args: List[String]) => (args, args.headOption))
        val flow  = start >>> next
        assertM(flow.run(List("Hello", "World")))(
          equalTo(OutputChannels(state = List("Hello", "World"), value = Option("Hello")))
        )
      },
      testM("It should be possible to combine flows using zip") {
        val flowA = Flow.succeed("A")
        val flowB = Flow.succeed(1)
        val flow  = flowA zip flowB
        assertM(flow.run)(equalTo(OutputChannels(state = ("A", 1), value = ("A", 1))))
      },
      testM("It should be possible to combine flows using the zip operator <*>") {
        val flowA = Flow.succeed("A")
        val flowB = Flow.succeed(1)
        val flow  = flowA <*> flowB
        assertM(flow.run)(equalTo(OutputChannels(state = ("A", 1), value = ("A", 1))))
      },
      testM("It should be possible to sequence flows using flatMap") {
        val flow = Flow.succeed("true").flatMap(value => Flow.succeed(s"The answer is: $value"))
        assertM(flow.run)(equalTo(OutputChannels.fromValue("The answer is: true")))
      },
      testM("It should be possible to sequence flows using a for comprehension") {
        for {
          _ <- TestSystem.putEnv("PROFILE", "local")
          out <- (for {
                   cfg             <- Flow.succeed(Map("profile.local.host" -> "127.0.0.1", "profile.default.host" -> "finos.org"))
                   selectedProfile <- Flow.fromEffect(zio.system.envOrElse("PROFILE", "default"))
                   host            <- Flow.succeed(cfg.getOrElse(s"profile.$selectedProfile.host", "morhir.org"))
                 } yield host).run.map(_.value)
        } yield assert(out)(equalTo("127.0.0.1"))
      }
    )
  )

  final case class Widget(name: String)
}
