package zio.morphir.ir.types

import zio.morphir.ir.{FQName, Name}
//import zio.morphir.sdk.ResultModule.Result

trait TypeModule extends TypeModuleSyntax {
  final type Type[+A] = zio.morphir.ir.types.Type[A]
  val Type: zio.morphir.ir.types.Type.type = zio.morphir.ir.types.Type

  final type Constructors[+A] = zio.morphir.ir.types.Constructors[A]
  val Constructors: zio.morphir.ir.types.Constructors.type = zio.morphir.ir.types.Constructors

  final type Definition[+A] = zio.morphir.ir.types.Definition[A]
  val Definition: zio.morphir.ir.types.Definition.type = zio.morphir.ir.types.Definition

  final type Field[+A] = zio.morphir.ir.types.Field[A]
  val Field: zio.morphir.ir.types.Field.type = zio.morphir.ir.types.Field

  final type Specification[+A] = zio.morphir.ir.types.Specification[A]
  val Specification: zio.morphir.ir.types.Specification.type = zio.morphir.ir.types.Specification

  final type UType = zio.morphir.ir.types.UType
  val UType: zio.morphir.ir.types.UType.type = zio.morphir.ir.types.UType

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

final object TypeModule extends TypeModule
