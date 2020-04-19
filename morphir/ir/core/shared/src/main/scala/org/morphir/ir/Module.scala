//package org.morphir.ir
//
//import org.morphir.ir.AccessControlled.AccessControlled
//import upickle.default.{readwriter, ReadWriter => RW}
//import upickle.default._
//
//object Module {
//  case class Declaration[X](
//      types: Map[Name, Type.Declaration[X]],
//      values: Map[Name, Value.Declaration[X]]
//  )
//
//  object Declaration {
//    implicit def readWriter[X: ReadWriter]: RW[Declaration[X]] =
//      readwriter[ujson.Value].bimap[Declaration[X]](
//        decl => ???,
//        json => ???
//      )
//  }
//
//  /**
//    * Type that represents a module definition. It includes types and values
//    *
//    * @param types a map of types by their names
//    * @param values a map of values by their names
//    */
//  case class Definition[X](
//      types: Map[Name, AccessControlled[Type.Definition[X]]],
//      values: Map[Name, AccessControlled[Value.Definition[X]]]
//  ) {
//    def toDeclaration: Declaration[X] = {
//      val defTypes = types.collect {
//        case (name, AccessControlled.Public(typeDef)) =>
//          name -> typeDef
//      }.toMap
//      val values = Map.empty[Name, Value.Definition[X]]
//      //Declaration(types, values)
//      ???
//    }
//  }
//
//  object Definition {
//    implicit def readWriter[X: ReadWriter]: RW[Definition[X]] =
//      readwriter[ujson.Value].bimap[Definition[X]](
//        decl => ???,
//        json => ???
//      )
//  }
//}
