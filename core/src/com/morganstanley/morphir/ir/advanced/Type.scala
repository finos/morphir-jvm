package com.morganstanley.morphir.ir.advanced
import com.morganstanley.morphir.ir.Name

sealed trait Type[+Extra]
object Type {
  case class Variable[A](name: Name, extra: A) extends Type[A]
  case class Function[A](argType: Type[A], returnType: Type[A], extra: A)
      extends Type[A]
  case class Unit[A](extra: A) extends Type[A]
}
