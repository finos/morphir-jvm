package morphir.flowz
import zio._

trait BehaviorEffectSyntax {
  import BehaviorEffectSyntax._
  implicit def convertZIOToBehaviorEffectOps[SIn, SOut, Msg, R, E, A](
    zio: ZIO[(SIn, Msg, R), E, BehaviorSuccess[SOut, A]]
  ): BehaviorEffectOps[SIn, SOut, Msg, R, E, A] =
    new BehaviorEffectOps(zio)

}

object BehaviorEffectSyntax extends BehaviorEffectSyntax {
  final class BehaviorEffectOps[SIn, SOut, Msg, R, E, A](
    private val zio: ZIO[(SIn, Msg, R), E, BehaviorSuccess[SOut, A]]
  ) extends AnyVal { self =>
    def provideState(initialState: SIn): BehaviorEffect[Any, SOut, Msg, R, E, A] =
      ZIO.accessM[(Any, Msg, R)] { case (_, msg, r) => zio.provide((initialState, msg, r)) }
  }
}
