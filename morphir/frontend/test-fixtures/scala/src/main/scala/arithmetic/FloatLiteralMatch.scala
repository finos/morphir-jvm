package arithmetic

object FloatLiteralMatch:
  def floatLiteralMatch(value: Float): Int =
    value match
      case 1.5f => 10
      case 2.5f => 20
      case _    => 0
