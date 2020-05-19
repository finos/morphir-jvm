package morphir.lang.scala
import morphir.ir.{ Name, TypeDefinition }

import scala.meta.Tree

object ScalaBackend {
  trait Service {
    //def rewrite[A, B](typeExpr: TypeExpr[A]): TypeExpr[B]
    def generate[A](name: Name, typeDef: TypeDefinition[A]): Tree
  }

  final case class Live() extends Service {
    //def rewrite[A, B](typeExpr: TypeExpr[A]): TypeExpr[B] = ???

    def generate[A](name: Name, typeDef: TypeDefinition[A]): Tree = {
      import _root_.scala.meta._

      val typeName = scala.meta.Type.Name(name.toCamelCase)

      q"case class $typeName ()"
    }
  }
}
