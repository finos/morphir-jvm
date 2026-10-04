package arithmetic

case class PersonWithAgeCheck(age: Int):
  def isAtLeast(threshold: Int): Boolean = age >= threshold
