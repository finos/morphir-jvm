package morphir.flowz

import zio._

trait Types {

  /**
   * The type of the core runtime environment of flowz.
   */
  type RuntimeEnv <: Has[_]

  /**
   * The type of the base environment of flowz.
   */
  type BaseEnv <: Has[_]
}
