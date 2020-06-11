package morphir.lang.scala
import morphir.ir.{ Name, Type => TypeExpr }

import scala.meta._
import morphir.ir.Type.Record

object ScalaBackend {
  trait Service {
    //def rewrite[A, B](typeExpr: TypeExpr[A]): TypeExpr[B]
    //def generate[A](name: Name, typeDef: TypeDefinition[A]): Tree
    def toTree[A](name: Name)(typeExpr: TypeExpr[A]): Tree
  }

  final case class Live() extends Service {
    //def rewrite[A, B](typeExpr: TypeExpr[A]): TypeExpr[B] = ???

    def toTree[A](name: Name)(typeExpr: TypeExpr[A]): Tree = typeExpr match {
      case Record(_, _) =>
        val typeName = scala.meta.Type.Name(name.toCamelCase)
        q"case class $typeName ()"
      case _ => ???
    }

  }
}
