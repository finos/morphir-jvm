package arithmetic

object ListMapTupleMatch:
  def sumPairs(values: List[(Int, Int)]): List[Int] =
    values.map(pair =>
      pair match
        case (left, right) => left + right
    )
