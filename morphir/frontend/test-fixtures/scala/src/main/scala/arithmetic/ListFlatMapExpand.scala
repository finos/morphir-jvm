package arithmetic

object ListFlatMapExpand:
  def expand(values: List[Int]): List[Int] =
    values.flatMap(value => List(value, value + 1))
