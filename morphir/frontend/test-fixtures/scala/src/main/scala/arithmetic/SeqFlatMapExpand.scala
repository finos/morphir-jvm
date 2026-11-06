package arithmetic

object SeqFlatMapExpand:
  def expand(values: Seq[Int]): Seq[Int] =
    values.flatMap(value => Seq(value, value + 1))
