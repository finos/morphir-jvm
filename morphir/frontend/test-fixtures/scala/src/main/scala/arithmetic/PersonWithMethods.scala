package arithmetic

case class PersonWithMethods(age: Int):
  def isAdult: Boolean = age >= 18

  def nextAge: Int = this.age + 1
