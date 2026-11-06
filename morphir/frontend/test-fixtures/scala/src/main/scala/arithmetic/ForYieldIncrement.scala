package arithmetic

object ForYieldIncrement:
  def increment(values: List[Int]): List[Int] =
    for value <- values
    yield value + 1
