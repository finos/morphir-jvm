package arithmetic

object HelperCall:
  def addThree(a: Int, b: Int, c: Int): Int =
    a + b + c

  def combine(a: Int, b: Int, c: Int): Int =
    addThree(a, b, c)
