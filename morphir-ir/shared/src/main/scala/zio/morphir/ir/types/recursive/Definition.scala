package zio.morphir.ir.types.recursive

import zio.Chunk
import zio.morphir.ir.{AccessControlled, FQName, Name}
//import zio.morphir.sdk.ResultModule.Result

sealed trait Definition[+Attributes] { self =>
  import Definition._
  import Specification._

  def collectReferences: Set[FQName] = self match {
    case TypeAlias(_, typeExp)                     => typeExp.collectReferences
    case CustomType(_, AccessControlled(_, value)) => value.collectReferences
  }

  def eraseAttributes: UDefinition = self match {
    case TypeAlias(typeParams, typeExp) =>
      TypeAlias(typeParams, typeExp.eraseAttributes)
    case CustomType(typeParams, ctors) =>
      CustomType(typeParams, ctors.map(_.eraseAttributes))
  }

//  def map[E, Attributes0](
//      f: Type[Attributes] => Result[E, Type[Attributes0]]
//  ): Result[List[E], Definition[Attributes0]] = self match {
//    case TypeAlias(typeParams, typeExp) =>
//      f(typeExp) match {
//        case Left(value)  => Left(List(value))
//        case Right(value) => Right(TypeAlias(typeParams, value))
//      }
//    case CustomType(typeParams, AccessControlled.WithPublicAccess(const)) =>
//      CustomType(
//        typeParams,
//        const.toMap.map { case (_, value) => value.map { case (name, typ) => (name, typ.mapAttributes(f)) } }
//      )
//  }

  def map[B](f: Attributes => B): Definition[B] = self match {
    case TypeAlias(typeParams, typeExp) => TypeAlias(typeParams, typeExp.map(f))
    case CustomType(typeParams, ctors)  => CustomType(typeParams, ctors.map(_.map(f)))
  }

  def toSpecification: Specification[Attributes] = self match {
    case TypeAlias(typeParams, typeExp) => TypeAliasSpecification(typeParams, typeExp)
    case CustomType(typeParams: Chunk[Name], AccessControlled.WithPublicAccess(ctors)) =>
      CustomTypeSpecification(typeParams, ctors)
    case CustomType(typeParams, _) =>
      OpaqueTypeSpecification(typeParams)
  }

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
