package zio.morphir.ir.types

import zio.morphir.ir.Name
import zio.prelude._

final case class Field[+T](name: Name, fieldType: T) { self =>

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
    Field(name, fieldType.mapAttributes(_ => attributes))

  /**
   * Attributes the field's type using the given function.
   */
  def attributeTypeWith[Attributes0, Attributes](f: Attributes0 => Attributes)(implicit
      ev: T <:< Type[Attributes0]
  ): Field[Type[Attributes]] =
    Field(name, fieldType.mapAttributes(f))

  def forEach[G[+_]: IdentityBoth: Covariant, U](f: T => G[U]): G[Field[U]] =
    f(self.fieldType).map(newType => self.copy(fieldType = newType))

  def map[U](f: T => U): Field[U] = Field(name, f(fieldType))

  def mapAttributes[Attributes0, Attributes1](f: Attributes0 => Attributes1)(implicit
      ev: T <:< Type[Attributes0]
  ): Field[Type[Attributes1]] =
    Field(name, fieldType.mapAttributes(f))

}
