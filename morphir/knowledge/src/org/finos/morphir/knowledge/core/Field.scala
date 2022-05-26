package org.finos.morphir.knowledge.core

import scala.reflect.ClassTag

// TODO: Consider if we want to use izumi.reflect.Tag.Tag instead of ClassTag
final case class Field[A](name: Name, fieldType: ClassTag[A])
object Field {
  def define[A](name: Name)(implicit tpe: ClassTag[A]): Field[A]            = Field(name, tpe)
  def define[A](implicit name: sourcecode.Name, tpe: ClassTag[A]): Field[A] = Field(name.value, tpe)
}
