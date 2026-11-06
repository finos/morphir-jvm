package arithmetic

object Clamp:
  def clamp(low: Int, high: Int, a: Int): Int =
    if a < low then low
    else if a > high then high
    else a
