package arithmetic

case class PersonWithinRange(age: Int):
  def adjustedAge(lowerBound: Int, upperBound: Int): Int =
    if age < lowerBound then lowerBound
    else if age > upperBound then upperBound
    else age
