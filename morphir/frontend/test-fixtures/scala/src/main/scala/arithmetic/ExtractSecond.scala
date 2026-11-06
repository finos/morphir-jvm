package arithmetic

object ExtractSecond:
  def extractSecond(pair: (Int, String)): String =
    pair match
      case (_, second) => second
