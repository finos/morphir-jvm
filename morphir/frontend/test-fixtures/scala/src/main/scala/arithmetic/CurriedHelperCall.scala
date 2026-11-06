package arithmetic

object CurriedHelperCall:
  def addThenAdd(a: Int, b: Int)(c: Int): Int =
    a + b + c

  def combineCurried(a: Int, b: Int, c: Int): Int =
    addThenAdd(a, b)(c)
