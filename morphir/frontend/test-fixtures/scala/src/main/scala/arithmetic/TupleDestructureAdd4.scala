package arithmetic

object TupleDestructureAdd4:
  def tupleDestructureAdd4(quadruple: (Int, Int, Int, Int)): Int =
    val (w, x, y, z) = quadruple
    w + x + y + z
