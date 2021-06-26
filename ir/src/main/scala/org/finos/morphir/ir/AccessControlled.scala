package org.finos.morphir.ir

final case class AccessControlled[+A](access:Access, value:A) {
    
    def getPrivate:Option[A] = access match {
        case Access.Public => None
        case Access.Private => Some(value)
    }

    def getPublic:Option[A] = access match {
        case Access.Public => Some(value)
        case Access.Private => None
    }

    def isPrivate:Boolean = access == Access.Private
    def isPublic:Boolean = access == Access.Public


    def map[B](f:A => B):AccessControlled[B] = 
        copy(value = f(value))
}

enum Access:
    case Public
    case Private