package arithmetic

object GetNamedIntensity:
  def getNamedIntensity(color: ColorWithTwoValues): Int =
    color match
      case ColorWithTwoValues.Named(_, intensity) => intensity
      case ColorWithTwoValues.Custom(red, green)  => red + green
