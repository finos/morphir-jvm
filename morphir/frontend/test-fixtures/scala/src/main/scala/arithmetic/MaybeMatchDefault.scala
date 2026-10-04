package arithmetic

object MaybeMatchDefault:
  def maybeMatchDefault(fallback: Int, maybeValue: Option[Int]): Int =
    maybeValue match
      case Some(value) => value
      case None        => fallback
