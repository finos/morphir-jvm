package arithmetic

object SeqMapIncrement:
  def increment(values: Seq[Int]): Seq[Int] =
    values.map(value => value + 1)
