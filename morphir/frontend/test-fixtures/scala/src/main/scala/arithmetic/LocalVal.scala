package arithmetic

object LocalVal:
  def addOne(value: Int): Int =
    val next = value + 1
    next
