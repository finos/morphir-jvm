package arithmetic

object GetPrimaryValue:
  def getPrimaryValue(color: ColorWithThreeValues): Int =
    color match
      case ColorWithThreeValues.Named(_, intensity, _)   => intensity
      case ColorWithThreeValues.Custom(red, green, blue) => red + green + blue
