package arithmetic

object ExtractFirstOf3:
  def extractFirstOf3(triple: (Int, String, Boolean)): Int =
    triple match
      case (first, _, _) => first
