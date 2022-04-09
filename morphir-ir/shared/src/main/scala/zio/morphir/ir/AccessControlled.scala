package zio.morphir.ir
import zio.prelude._

import AccessControlled.Access

final case class AccessControlled[+A](access: Access, value: A) { self =>
  def map[B](f: A => B): AccessControlled[B] =
    AccessControlled(access, f(value))

  def flatMap[B](f: A => AccessControlled[B]): AccessControlled[B] =
    f(value)

  def fold[B](ifPublic: A => B, ifPrivate: A => B): B =
    access match {
      case Access.Public  => ifPublic(self.value)
      case Access.Private => ifPrivate(self.value)
    }

  def withPublicAccess: Option[A] = self match {
    case AccessControlled(Access.Public, a) => Some(a)
    case _                                  => None
  }

  /**
   * Get the value with private access level. Will always return the value.
   */
  def withPrivateAccess: A = self match {
    case AccessControlled(Access.Public, a)  => a
    case AccessControlled(Access.Private, a) => a
  }
}

object AccessControlled {

  def publicAccess[A](value: A): AccessControlled[A]  = AccessControlled(Access.Public, value)
  def privateAccess[A](value: A): AccessControlled[A] = AccessControlled(Access.Private, value)

  sealed trait Access
  object Access {
    case object Public  extends Access
    case object Private extends Access
  }

  object WithPrivateAccess {
    def unapply[A](accessControlled: AccessControlled[A]): Option[A] =
      Some(accessControlled.withPrivateAccess)
  }
  object WithPublicAccess {
    def unapply[A](accessControlled: AccessControlled[A]): Option[A] =
      accessControlled.withPublicAccess
  }

  implicit val AccessControlledCovariant: Covariant[AccessControlled] = new Covariant[AccessControlled] {
    def map[A, B](f: A => B): AccessControlled[A] => AccessControlled[B] = _.map(f)
  }

  implicit val AccessControlledForEach: ForEach[AccessControlled] =
    new ForEach[AccessControlled] {
      def forEach[G[+_]: IdentityBoth: Covariant, A, B](self: AccessControlled[A])(
          f: A => G[B]
      ): G[AccessControlled[B]] =
        f(self.value).map(AccessControlled(self.access, _))
    }
}
