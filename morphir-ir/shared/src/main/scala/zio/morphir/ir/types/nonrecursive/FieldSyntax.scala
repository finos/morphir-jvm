package zio.morphir.ir.types.nonrecursive

import zio.morphir.ir.Name

trait FieldSyntax {
  final def defineField(name: Name, fieldType: UType): Field[UType] = Field(name, fieldType)

  final def defineField(name: String, fieldType: UType): Field[UType] = Field(Name.fromString(name), fieldType)

  final def field[A](name: String, tpe: Type[A]): Field[Type[A]] = Field(Name.fromString(name), tpe)
  final def field[A](name: Name, tpe: Type[A]): Field[Type[A]]   = Field(name, tpe)

}
