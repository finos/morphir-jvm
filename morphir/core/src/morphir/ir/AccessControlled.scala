package morphir.ir
sealed trait AccessControlled[+A] {

  def withPublicAccess: Option[A] = this match {
    case AccessControlled.Public(v) => Some(v)
    case _                          => None
  }

  def withPrivateAccess: A = this match {
    case AccessControlled.Public(v)  => v
    case AccessControlled.Private(v) => v
  }
}

object AccessControlled {

  case class Public[+A] private (value: A) extends AccessControlled[A]
  case class Private[+A] private (value: A) extends AccessControlled[A]

  def `public`[A](value: A): Public[A] =
    Public(value)

  def `private`[A](value: A): Private[A] =
    Private(value)

  @inline def withPublicAccess[A](ac: AccessControlled[A]): Option[A] =
    ac.withPublicAccess

  @inline def withPrivateAccess[A](ac: AccessControlled[A]): A =
    ac.withPrivateAccess
}
