package arithmetic

object MaybeMatchMap:
  def maybeMatchMap(maybeValue: Option[Int]): Option[Int] =
    maybeValue match
      case Some(value) => Some(value + 1)
      case None        => None
