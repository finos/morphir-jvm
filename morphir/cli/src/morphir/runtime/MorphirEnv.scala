package morphir.runtime

import zio._
import zio.console._
import zio.internal.Platform

object MorphirEnv {
  val platform: Platform = Platform.default

  val default: MorphirEnv =
    Runtime.unsafeFromLayer(ZEnv.live, platform).environment
}
