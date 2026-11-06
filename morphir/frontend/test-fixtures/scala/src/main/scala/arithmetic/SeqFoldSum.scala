package arithmetic

object SeqFoldSum:
  def total(values: Seq[Int]): Int =
    values.foldLeft(0)((acc, value) => acc + value)
