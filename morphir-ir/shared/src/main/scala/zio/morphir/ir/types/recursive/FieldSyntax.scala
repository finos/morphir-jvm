package zio.morphir.ir.types.recursive
import zio.morphir.ir.Name

import Type.FieldT

trait FieldSyntax {
  final def defineField(name: Name, fieldType: UType): Field[UType] = Field(name, fieldType)

  final def defineField(name: String, fieldType: UType): Field[UType] = Field(Name.fromString(name), fieldType)

  final def field[A](name: String, tpe: Type[A]): FieldT[A] = Field(Name.fromString(name), tpe)
  final def field[A](name: Name, tpe: Type[A]): FieldT[A]   = Field(name, tpe)

  final def field[A](tuple: (String, Type[A])): FieldT[A] = Field(Name.fromString(tuple._1), tuple._2)

}
