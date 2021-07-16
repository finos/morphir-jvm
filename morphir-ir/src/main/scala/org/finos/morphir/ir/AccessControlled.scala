package org.finos.morphir.ir

final case class AccessControlled[+A](access: Access, value: A) {

  def isPrivate: Boolean = access == Access.Private
  def isPublic: Boolean  = access == Access.Public

  def map[B](f: A => B): AccessControlled[B] =
    copy(value = f(value))

  def withPrivateAccess: A = access match {
    case Access.Public  => value
    case Access.Private => value
  }

  def withPublicAccess: Option[A] = access match {
    case Access.Public  => Some(value)
    case Access.Private => None
  }
}

enum Access:
  case Public
  case Private
