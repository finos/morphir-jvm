package arithmetic

object MaybeMatchIncrement:
  def maybeMatchIncrement(maybeValue: Option[Int]): Int =
    maybeValue match
      case Some(value) => value + 1
      case None        => 0
