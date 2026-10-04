package arithmetic

object ListMapIncrement:
  def incrementAll(values: List[Int]): List[Int] =
    values.map(value => value + 1)
