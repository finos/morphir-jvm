package zio.morphir.ir.types.nonrecursive

import zio.morphir.ir.Name
import zio.prelude._

final case class Field[+T](name: Name, data: T) { self =>

  /**
   * An alias for `attributeTypeWith`.
   */
  def @@[Attributes0, Attributes](f: Attributes0 => Attributes)(implicit
      ev: T <:< Type[Attributes0]
  ): Field[Type[Attributes]] =
    attributeTypeWith(f)

  /**
   * Attributes the field with the given `attributes`.
   */
  def attributeTypeAs[Attributes](attributes: => Attributes)(implicit ev: T <:< Type[_]): Field[Type[Attributes]] =
    Field(name, data.mapAttributes(_ => attributes))

  /**
   * Attributes the field's type using the given function.
   */
  def attributeTypeWith[Attributes0, Attributes](f: Attributes0 => Attributes)(implicit
      ev: T <:< Type[Attributes0]
  ): Field[Type[Attributes]] =
    Field(name, data.mapAttributes(f))

  def forEach[G[+_]: IdentityBoth: Covariant, U](f: T => G[U]): G[Field[U]] =
    f(self.data).map(newType => self.copy(data = newType))

  def map[U](f: T => U): Field[U] = Field(name, f(data))

  def mapAttributes[Attributes0, Attributes1](f: Attributes0 => Attributes1)(implicit
      ev: T <:< Type[Attributes0]
  ): Field[Type[Attributes1]] =
    Field(name, data.mapAttributes(f))

}

object Field extends FieldSyntax {

  def apply[T](name: String, data: T): Field[T] = Field(Name.fromString(name), data)

  final implicit class FieldOfType[A](private val self: Field[Type[A]]) extends AnyVal {
    def fieldType: Type[A] = self.data
  }
}
