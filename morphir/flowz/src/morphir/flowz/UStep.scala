package morphir.flowz

object UStep {

  /**
   * Returns a step with the empty value.
   */
  val none: UStep[Any, Option[Nothing]] = Flow.none

  /**
   * A step that succeeds with a unit value.
   */
  val unit: UStep[Any, Unit] = Flow.unit
}
