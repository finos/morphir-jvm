package morphir.core.newtype

sealed trait NewtypeModule {
  def newtype[A]: Newtype[A]
  def subtype[A]: Subtype[A]

  sealed trait Newtype[A] {
    type WrappedType
    def apply(a: A): WrappedType
    def unwrap(wt: WrappedType): A
    def unapply(wt: WrappedType): Option[A] =
      Some(unwrap(wt))
    def toF[F[_]](fa: F[A]): F[WrappedType]
    def fromF[F[_]](fa: F[WrappedType]): F[A]
  }

  sealed trait Subtype[A] extends Newtype[A] {
    type WrappedType <: A
  }
}

object NewtypeModule {
  val instance: NewtypeModule = new NewtypeModule {
    def newtype[A]: Newtype[A] = new Newtype[A] {
      type WrappedType = A
      def apply(a: A): WrappedType                       = a
      def unwrap(wt: WrappedType): A                     = wt
      override def toF[F[_]](fa: F[A]): F[WrappedType]   = fa
      override def fromF[F[_]](fa: F[WrappedType]): F[A] = fa
    }

    def subtype[A]: Subtype[A] = new Subtype[A] {
      type WrappedType = A
      def apply(a: A): WrappedType                       = a
      def unwrap(wt: WrappedType): A                     = wt
      override def toF[F[_]](fa: F[A]): F[WrappedType]   = fa
      override def fromF[F[_]](fa: F[WrappedType]): F[A] = fa
    }
  }
}

trait NewtypeModuleExports {
  import NewtypeModule._

  abstract class Newtype[A] extends instance.Newtype[A] {
    val newtype: instance.Newtype[A] = instance.newtype[A]
    type WrappedType = newtype.WrappedType
    def apply(a: A): WrappedType                       = newtype(a)
    def unwrap(wt: WrappedType): A                     = newtype.unwrap(wt)
    override def toF[F[_]](fa: F[A]): F[WrappedType]   = newtype.toF(fa)
    override def fromF[F[_]](fa: F[WrappedType]): F[A] = newtype.fromF(fa)
  }

  abstract class Subtype[A] extends instance.Subtype[A] {
    val subtype: instance.Subtype[A] = instance.subtype[A]
    type WrappedType = subtype.WrappedType
    def apply(a: A): WrappedType                       = subtype(a)
    def unwrap(wt: WrappedType): A                     = subtype.unwrap(wt)
    override def toF[F[_]](fa: F[A]): F[WrappedType]   = subtype.toF(fa)
    override def fromF[F[_]](fa: F[WrappedType]): F[A] = subtype.fromF(fa)
  }
}
