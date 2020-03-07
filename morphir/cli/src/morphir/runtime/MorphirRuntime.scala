package morphir.runtime

import zio._
import zio.console._
import zio.internal.Platform

trait MorphirRuntime extends Runtime[MorphirEnv] {
  override val environment: MorphirEnv = MorphirEnv.default

  override val platform: Platform = MorphirEnv.platform
}
