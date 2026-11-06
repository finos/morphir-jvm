package arithmetic

object IntLiteralMatch:
  def intLiteralMatch(value: Int): Int =
    value match
      case 0 => 100
      case 1 => 200
      case _ => value + 1
