package morphir.flowz.platform
import zio._
import zio.stream._

final case class Worker[R, E, Flags, Model, Msg](
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
          .foreach(loop(_))

      cmd.messages.foreach(loop(_))
    }
  }
}

object Worker {}
