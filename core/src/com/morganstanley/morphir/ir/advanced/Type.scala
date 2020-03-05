package com.morganstanley.morphir.ir.advanced
import com.morganstanley.morphir.ir.{FQName, Name}

sealed trait Type[+Extra]
object Type {
  case class Variable[A](name: Name, extra: A) extends Type[A]
  case class Reference[A](
      typeName: FQName,
      typeParameters: List[Type[A]],
      extra: A
  ) extends Type[A]

  case class Tuple[A](elementTypes: List[Type[A]], extra: A) extends Type[A]
  case class Record[A](fieldTypes: List[Field[A]], extra: A) extends Type[A]
  case class ExtensibleRecord[A]() extends Type[A]
  case class Function[A](argType: Type[A], returnType: Type[A], extra: A)
      extends Type[A]
  case class Unit[A](extra: A) extends Type[A]

  case class Field[A](name: Name, fieldType: Type[A])
}
