package morphir.flowz.platform
import zio._
import zio.stream._

final case class Worker[-R, E, -Flags, Model, Msg](
  init: Flags => (Model, Cmd[R, E, Msg]),
  update: (Msg, Model) => ZIO[R, E, (Model, Cmd[R, E, Msg])],
  subscriptions: Model => Sub[R, E, Msg]
) {

  def run(flags: Flags): ZIO[R, E, Unit] = {
    val (model, cmd) = init(flags)

    Ref.make[Model](model).flatMap { ref =>
      def loop(msg: Msg): ZIO[R, E, Unit] =
        ZStream
          .fromEffect(ref.get)
          .flatMap { model =>
            ZStream.fromEffect(update(msg, model)).flatMap { case (model2, cmd) =>
              ZStream.empty.ensuring(ref.set(model2)) ++ (cmd.messages merge subscriptions(model2).messages)
            }
          }
          .foreach(loop)

      cmd.messages.foreach(loop)
    }
  }
}

object Worker {}

object workerExample extends App {
  final case class Model(counter: Int)
  sealed abstract class Msg
  object Msg {
    final case class Reset(value: Int) extends Msg
    case object Decrement              extends Msg
  }

  val simpleWorker = Worker(
    init = (_: List[String]) => (Model(10), Cmd.ofMsg(Msg.Reset(20))),
    update = (message: Msg, model: Model) =>
      (message, model) match {
        case (Msg.Reset(value), _) =>
          console.putStrLn(s"Reset to: $value") *> ZIO.succeed((Model(value), Cmd.ofMsg(Msg.Decrement)))
        case (Msg.Decrement, m @ Model(0)) => console.putStrLn("All done") *> ZIO.succeed((m, Cmd.none))
        case (Msg.Decrement, Model(value)) =>
          console.putStrLn(s"Decrementing to: ${value - 1}") *> ZIO.succeed(
            (Model(value - 1), Cmd.ofMsg(Msg.Decrement))
          )
      },
    subscriptions = (_: Model) => Sub.none
  )

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    simpleWorker.run(args).exitCode
}
