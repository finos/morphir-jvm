package arithmetic

object ListFilterPositive:
  def keepPositive(values: List[Int]): List[Int] =
    values.filter(value => value > 0)
