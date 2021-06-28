package org.finos.morphir.ir

/**
 * Type that represents a documented value.
 */ 
final case class Documented[+A](doc:String, value:A):
    def map[B](f:A => B):Documented[B] = copy(value = f(value))