package morphir.flowz.platform

import zio.console
import zio.test._
import zio.test.Assertion._

object WorkerSpec extends DefaultRunnableSpec { self =>
  def spec = suite("Worker Specs")(
    testM("It should be possible to run a simple worker")(
      for {
        _   <- console.putStrLn(s"Running...")
        res <- simpleWorker.run(List("Hello", "World"))
      } yield assert(res)(isUnit)
    )
  )

  val simpleWorker = Worker(
    init = (args: List[String]) => (Model(args), Cmd.none),
    update = (_: Unit, model: Model) =>
      for {
        _ <- console.putStrLn(s"Model is: $model")
      } yield (model, Cmd.unit),
    subscriptions = (_: Model) => Sub.none
  )

  final case class Model(args: List[String])

}
