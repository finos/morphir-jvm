package arithmetic

object BooleanLiteralMatch:
  def booleanLiteralMatch(value: Boolean): Int =
    value match
      case true  => 1
      case false => 0
