package arithmetic

object StringLiteralMatch:
  def stringLiteralMatch(value: String): Int =
    value match
      case "vip" => 10
      case _     => 0
