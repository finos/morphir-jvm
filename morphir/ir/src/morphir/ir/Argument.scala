package morphir.ir

object Argument {
  def apply[A](argName: Name, attributes: A): Argument[A] = (argName, attributes)
  def fromTuple[A](value: (Name, A)): Argument[A]         = value
}
