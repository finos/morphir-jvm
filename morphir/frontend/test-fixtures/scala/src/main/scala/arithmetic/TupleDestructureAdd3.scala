package arithmetic

object TupleDestructureAdd3:
  def tupleDestructureAdd3(triple: (Int, Int, Int)): Int =
    val (x, y, z) = triple
    x + y + z
