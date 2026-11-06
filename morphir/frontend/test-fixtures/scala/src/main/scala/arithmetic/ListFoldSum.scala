package arithmetic

object ListFoldSum:
  def sum(values: List[Int]): Int =
    values.foldLeft(0)((acc, value) => acc + value)
