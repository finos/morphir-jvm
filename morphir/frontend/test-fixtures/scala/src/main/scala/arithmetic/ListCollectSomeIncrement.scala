package arithmetic

object ListCollectSomeIncrement:
  def collectDefined(values: List[Option[Int]]): List[Int] =
    values.collect { case Some(value) =>
      value + 1
    }
