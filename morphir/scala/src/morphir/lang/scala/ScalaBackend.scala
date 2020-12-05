/*
Copyright 2020 Morgan Stanley

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 */

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
          case morphir.ir.Type.Field(name, morphir.ir.Type.Variable(_, typeVarName)) =>
            println(s"TypeName: $typeName")
            Term.Param(
              List.empty,
              scala.meta.Name(name.toCamelCase),
              Some(typeVarName.toTypeName),
              None
            )
          case _ => ???
        }
        q"case class $typeName (..$paramss)"
      case _ => ???
    }

  }
}
