package arithmetic

object LocalValChain:
  def sumAndDouble(a: Int, b: Int): Int =
    val sum     = a + b
    val doubled = sum + sum
    doubled
