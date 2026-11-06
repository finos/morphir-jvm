package arithmetic

object GetIntensity:
  def getIntensity(color: ColorWithValue): Int =
    color match
      case ColorWithValue.Red(i)  => i
      case ColorWithValue.Blue(s) => s * 2
