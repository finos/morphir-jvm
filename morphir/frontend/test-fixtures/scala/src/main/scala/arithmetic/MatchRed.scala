package arithmetic

object MatchRed:
  def matchRed(color: Color): Boolean =
    color match
      case Color.Red  => true
      case Color.Blue => false
