package arithmetic

object TupleDestructureAdd:
  def tupleDestructureAdd(pair: (Int, Int)): Int =
    val (x, y) = pair
    x + y
