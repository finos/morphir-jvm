package com.morganstanley.morphir.ir.advanced

sealed trait Type[+Extra]
object Type {
    case class Function[A](argType:Type[A], returnType:Type[A], extra:A) extends Type[A]
    case class Unit[A](extra:A) extends Type[A]
}