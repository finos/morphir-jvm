package morphir.flowz
import zio._

trait ZBehaviorSyntax {
  import ZBehaviorSyntax._
  implicit def convertZIOToBehaviorOps[SIn, SOut, InputMsg, R, E, A](
    zio: ZIO[(SIn, InputMsg, R), E, StepSuccess[SOut, A]]
  ): ZBehaviorOps[SIn, SOut, InputMsg, R, E, A] =
    new ZBehaviorOps(zio)

}

object ZBehaviorSyntax extends ZBehaviorSyntax {
  final class ZBehaviorOps[SIn, SOut, InputMsg, R, E, A](
    private val zio: ZBehavior[SIn, SOut, InputMsg, R, E, A]
  ) extends AnyVal { self =>
    def provideState(initialState: SIn): ZBehavior[Any, SOut, InputMsg, R, E, A] =
      ZIO.accessM[(Any, InputMsg, R)] { case (_, msg, r) => zio.provide((initialState, msg, r)) }
  }
}
