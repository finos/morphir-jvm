package arithmetic

object ExtractFirst:
  def extractFirst(pair: (Int, String)): Int =
    pair match
      case (first, _) => first
