package arithmetic

case class PersonWithAdjustedAge(age: Int):
  def adjustedAge(threshold: Int): Int =
    if age + 1 > threshold then age + 1
    else threshold
