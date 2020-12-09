package morphir.flowz

import zio._

trait Api extends Types with Channels with Context {}

/**
 * The default way of using flowz.
 */
object default extends Api {
  override type RuntimeEnv = ZEnv
  override type BaseEnv    = RuntimeEnv
}
