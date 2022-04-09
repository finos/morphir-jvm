package zio.morphir.ir.types.recursive

import zio.morphir.ir.Name
import zio.prelude._

final case class Field[+T](name: Name, data: T) { self =>

  def forEach[G[+_]: IdentityBoth: Covariant, U](f: T => G[U]): G[Field[U]] =
    f(self.data).map(newType => self.copy(data = newType))

  def map[U](f: T => U): Field[U] = Field(name, f(data))

}

object Field extends FieldSyntax {

  def apply[T](name: String, data: T): Field[T] = Field(Name.fromString(name), data)

  final implicit class FieldOfType[A](private val self: Field[Type[A]]) extends AnyVal {
    def fieldType: Type[A] = self.data

    /**
     * Attributes the field with the given `attributes`.
     */
    def attributeTypeAs[Attributes](attributes: => Attributes): Field[Type[Attributes]] =
      Field(self.name, self.data.mapAttributes(_ => attributes))

    /**
     * Attributes the field's type using the given function.
     */
    def attributeTypeWith[B](f: A => B): Field[Type[B]] =
      Field(self.name, self.data.mapAttributes(f))

    def mapAttributes[B](f: A => B): Field[Type[B]] =
      Field(self.name, self.data.mapAttributes(f))
  }

}
