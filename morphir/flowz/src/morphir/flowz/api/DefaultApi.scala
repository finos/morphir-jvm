package morphir.flowz.api

import morphir.flowz.Api
import zio.ZEnv

abstract class DefaultApi extends Api {
  override type RuntimeEnv = ZEnv
  override type BaseEnv    = RuntimeEnv
}
