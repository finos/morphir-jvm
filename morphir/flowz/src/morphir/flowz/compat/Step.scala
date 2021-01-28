package morphir.flowz.compat

import morphir.flowz.{ Step => Step_ }
object Step {

  /**
   * Returns a step that models success with the specified value.
   */
  def succeed[A](value: => A): UStep[A] =
    Step_.succeed(value)

  val unit: UStep[Unit] = Step_.succeed(())
}
