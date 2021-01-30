/**
 * Represents a Stage in processing.
 * A stage also knows how to handle state as a first class concept.
 */
final case class Stage[-StateIn, +StateOut, -Env, -Params, +Err, +A](
  name: Option[String] = None,
  description: Option[String] = None
)            { self => }
object Stage {}
