package morphir.ir

import morphir.ir.codec.argumentCodecs

object argument {

  def apply[A](name: Name, value: A): Argument[A] = Argument(name, value)

  final case class Argument[+A](name: Name, value: A) {

    def map[B](f: (Name, A) => (Name, B)): Argument[B] = {
      val (newName, newValue) = f(name, value)
      Argument(newName, newValue)
    }

    def mapValue[B](f: A => B): Argument[B] = Argument(name, f(value))
  }

  object Argument extends argumentCodecs.ArgumentCodec {
    def fromTuple[A](tuple: (Name, A)): Argument[A] = Argument(tuple._1, tuple._2)
  }

  type ArgumentList[+A] = List[Argument[A]]

  implicit class ArgumentListOps[A](private val self: ArgumentList[A]) extends AnyVal {
    def mapValue[B](f: A => B): ArgumentList[B] = self.map(arg => arg.mapValue(f))
  }
}
