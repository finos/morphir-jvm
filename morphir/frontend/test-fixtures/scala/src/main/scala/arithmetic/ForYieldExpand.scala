package arithmetic

object ForYieldExpand:
  def expand(values: List[Int]): List[Int] =
    for
      value <- values
      next  <- List(value, value + 1)
    yield next
