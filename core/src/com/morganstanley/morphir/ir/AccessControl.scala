package com.morganstanley.morphir.ir

object AccessControl {
    sealed trait AccessControlled[+A]
    case class Public[+A] private (value:A) extends AccessControlled[A]
    case class Private[+A] private(value:A) extends AccessControlled[A]

    def `public`[A](value:A):Public[A] =
        Public(value)
    
    def `private`[A](value:A):Private[A] =
        Private(value)
}