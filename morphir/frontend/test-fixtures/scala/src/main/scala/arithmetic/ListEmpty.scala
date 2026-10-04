package arithmetic

object ListEmpty:
  def emptyViaFactory: List[Int] = List()

  def emptyViaNil: List[Int] = Nil

  def defaultList(input: Option[List[Int]]): List[Int] =
    input match
      case Some(values) => values
      case None         => List()
