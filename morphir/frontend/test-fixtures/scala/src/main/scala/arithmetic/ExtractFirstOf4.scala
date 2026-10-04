package arithmetic

object ExtractFirstOf4:
  def extractFirstOf4(quadruple: (Int, String, Boolean, Float)): Int =
    quadruple match
      case (first, _, _, _) => first
