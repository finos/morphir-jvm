package morphir.lang.scala

import morphir.ir.{ Type => TypeExpr }

object ScalaBackend {
  trait Service {
    def rewrite[A, B](typeExpr: TypeExpr[A]): TypeExpr[B]
    def generate[A](typeExpr: TypeExpr[A]): Any
  }

  final case class Live() extends Service {
    def rewrite[A, B](typeExpr: TypeExpr[A]): TypeExpr[B] = ???

    def generate[A](typeExpr: TypeExpr[A]): Any = ???
  }
}
