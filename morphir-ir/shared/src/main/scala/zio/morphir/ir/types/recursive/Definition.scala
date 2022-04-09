package zio.morphir.ir.types.recursive

import zio.Chunk
import zio.morphir.ir.{AccessControlled, Name}
//import zio.morphir.sdk.ResultModule.Result

sealed trait Definition[+Attributes] { self =>
  import Definition._
  import Specification._

  // def map[B]

  def eraseAttributes: UDefinition = ???

  def toSpecification: Specification[Attributes] = self match {
    case TypeAlias(typeParams, typeExp) => TypeAliasSpecification(typeParams, typeExp)
    case CustomType(typeParams: Chunk[Name], AccessControlled.WithPublicAccess(ctors)) =>
      CustomTypeSpecification(typeParams, ctors)
    case CustomType(typeParams, _) =>
      OpaqueTypeSpecification(typeParams)
  }

  def map[B](f: Attributes => B): Definition[B] = ???

//  def map[E, Attributes0](
//      f: Type[Attributes] => Result[E, Type[Attributes0]]
//  ): Result[List[E], Definition[Attributes0]] = self match {
//    case TypeAlias(typeParams, typeExp) => ???
//      Result.map(TypeAlias(typeParams, typeExp))
//    case CustomType(typeParams, AccessControlled.WithPublicAccess(const)) => ???
//      CustomType(typeParams, const.toMap.map{case (_, value) => value.map{case (name, typ) => (name, typ.mapAttributes(f))}})
//  }

//  def map[Attributes0](f: Attributes => Attributes0): Definition[Attributes0] = self match {
//    case TypeAlias(typeParams, typeExp) =>
//      TypeAlias(typeParams, typeExp.mapAttributes(f))
//    case CustomType(typeParams, AccessControlled.WithPublicAccess(const)) => ???
////      CustomType(typeParams, const.toMap.map{case (_, value) => value.map{case (name, typ) => (name, typ.mapAttributes(f))}})
//  }

  // def eraseAttributes: Definition[Nothing] = self match {
  //   case TypeAlias(typeParams, typeExp) =>
  //     TypeAlias(typeParams, typeExp.eraseAttributes)
  //   case CustomType(typeParams, ctors) =>
  //     CustomType(typeParams, ctors.eraseAttributes)
  // }

}

object Definition {
  type UDefinition = Definition[Any]
  val UDefinition: Definition.type = Definition

  final case class TypeAlias[+Attributes](typeParams: Chunk[Name], typeExp: Type[Attributes])
      extends Definition[Attributes]

  final case class CustomType[+Attributes](
      typeParams: Chunk[Name],
      ctors: AccessControlled[Constructors[Attributes]]
  ) extends Definition[Attributes]

  final implicit class DefinitionExtensions[A](private val self: Definition[A]) extends AnyVal {
    // def mapAttributes[B](f:A => B)
  }
}
