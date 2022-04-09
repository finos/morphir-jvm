package zio.morphir.ir.types.nonrecursive

import zio.morphir.ir.{FQName, Name}
//import zio.morphir.sdk.ResultModule.Result

trait TypeModule extends TypeModuleSyntax {
  final type Type[+A] = zio.morphir.ir.types.nonrecursive.Type[A]
  val Type: zio.morphir.ir.types.nonrecursive.Type.type = zio.morphir.ir.types.nonrecursive.Type

  final type Constructors[+A] = zio.morphir.ir.types.nonrecursive.Constructors[A]
  val Constructors: zio.morphir.ir.types.nonrecursive.Constructors.type = zio.morphir.ir.types.nonrecursive.Constructors

  final type Definition[+A] = zio.morphir.ir.types.nonrecursive.Definition[A]
  val Definition: zio.morphir.ir.types.nonrecursive.Definition.type = zio.morphir.ir.types.nonrecursive.Definition

  final type Field[+A] = zio.morphir.ir.types.nonrecursive.Field[A]
  val Field: zio.morphir.ir.types.nonrecursive.Field.type = zio.morphir.ir.types.nonrecursive.Field

  final type Specification[+A] = zio.morphir.ir.types.nonrecursive.Specification[A]
  val Specification: zio.morphir.ir.types.nonrecursive.Specification.type =
    zio.morphir.ir.types.nonrecursive.Specification

  final type UConstructors = zio.morphir.ir.types.nonrecursive.Constructors[Any]
  val UConstructors: zio.morphir.ir.types.nonrecursive.Constructors.type =
    zio.morphir.ir.types.nonrecursive.Constructors

  final type UType = zio.morphir.ir.types.nonrecursive.UType
  val UType: zio.morphir.ir.types.nonrecursive.UType.type = zio.morphir.ir.types.nonrecursive.UType

  final def definitionToSpecification[A](definition: Definition[A]): Specification[A] =
    definition.toSpecification

  final def mapSpecificationAttributes[A, B](f: A => B, spec: Specification[A]): Specification[B] = ???
//    spec.map(f)

//  def mapDefinition[A, B, E](
//      f: Type[A] => Result[E, Type[B]],
//      definition: Definition[A]
//  ): Result[List[E], Definition[B]] =
//    definition.map(f)

  def mapDefinitionAttributes[A, B](f: A => B, defn: Definition[A]): Definition[B] = ??? // defn.map(f)

  def mapTypeAttributes[A, B](f: A => B, tpe: Type[A]): Type[B] = tpe.mapAttributes(f)

  def typeAttributes[A](tpe: Type[A]): A = tpe.attributes

  def eraseAttributes[A](typeDef: Definition[A]): UDefinition = ??? // typeDef.eraseAttributes

  def collectVariables[A](tpe: Type[A]): Set[Name]    = tpe.collectVariables
  def collectReferences[A](tpe: Type[A]): Set[FQName] = tpe.collectReferences
  def substituteTypeVariables[A](mapping: Map[Name, Type[A]], tpe: Type[A]): Type[A] =
    ??? // tpe.substituteTypeVariables

  def toString[A](tpe: Type[A]): String = tpe.toString
}

object TypeModule extends TypeModule
