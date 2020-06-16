package morphir.lang.scala

import morphir.ir.{ Name, Type => TypeExpr }
import morphir.lang.scala.Naming._
import scala.meta.{ Type => MType, _ }

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
      case Record(_, fieldTypes) =>
        val typeName = MType.Name(name.toCamelCase)
        val paramss: List[Term.Param] = fieldTypes.map {
          case morphir.ir.Type.Field(name, morphir.ir.Type.Reference(_, fullyQualifiedTypeName, Nil)) =>
            println(s"TypeName: $typeName")
            Term.Param(
              List.empty,
              scala.meta.Name(name.toCamelCase),
              Some(fullyQualifiedTypeName.toTypeRef),
              None
            )
          case _ => ???
        }
        q"case class $typeName (..$paramss)"
      case _ => ???
    }

  }
}
