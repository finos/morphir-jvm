package arithmetic

object LocalValHelperCall:
  def addThree(a: Int, b: Int, c: Int): Int =
    a + b + c

  def addOneMore(a: Int, b: Int, c: Int): Int =
    val total = addThree(a, b, c)
    total + 1
