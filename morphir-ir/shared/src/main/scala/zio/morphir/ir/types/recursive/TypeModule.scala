package zio.morphir.ir.types.recursive

import zio.morphir.ir.{FQName, Name}
//import zio.morphir.sdk.ResultModule.Result

trait TypeModule extends AllTypeSyntax {
  final type Type[+A] = zio.morphir.ir.types.recursive.Type[A]
  val Type: zio.morphir.ir.types.recursive.Type.type = zio.morphir.ir.types.recursive.Type

  final type Constructors[+A] = zio.morphir.ir.types.recursive.Constructors[A]
  val Constructors: zio.morphir.ir.types.recursive.Constructors.type = zio.morphir.ir.types.recursive.Constructors

  final type Definition[+A] = zio.morphir.ir.types.recursive.Definition[A]
  val Definition: zio.morphir.ir.types.recursive.Definition.type = zio.morphir.ir.types.recursive.Definition

  final type Field[+A] = zio.morphir.ir.types.recursive.Field[A]
  val Field: zio.morphir.ir.types.recursive.Field.type = zio.morphir.ir.types.recursive.Field

  final type Specification[+A] = zio.morphir.ir.types.recursive.Specification[A]
  val Specification: zio.morphir.ir.types.recursive.Specification.type = zio.morphir.ir.types.recursive.Specification

  final type UDefinition = zio.morphir.ir.types.recursive.Definition[Any]

  final type UType = zio.morphir.ir.types.recursive.UType
  val UType: zio.morphir.ir.types.recursive.UType.type = zio.morphir.ir.types.recursive.UType

  final def definitionToSpecification[A](definition: Definition[A]): Specification[A] = definition.toSpecification

  final def mapSpecificationAttributes[A, B](f: A => B)(spec: Specification[A]): Specification[B] = spec.map(f)
  final def mapSpecificationAttributes[A](spec: Specification[A]): Specification.MapSpecificationAttributes[A] =
    Specification.mapSpecificationAttributes(spec)

//    spec.map(f)

//  def mapDefinition[A, B, E](
//      f: Type[A] => Result[E, Type[B]],
//      definition: Definition[A]
//  ): Result[List[E], Definition[B]] =
//    definition.map(f)

  def mapDefinitionAttributes[A, B](f: A => B, defn: Definition[A]): Definition[B] = ??? // defn.map(f)

  final def mapTypeAttributes[A, B](f: A => B, tpe: Type[A]): Type[B]     = tpe.mapAttributes(f)
  final def mapTypeAttributes[A](tpe: Type[A]): Type.MapTypeAttributes[A] = Type.mapTypeAttributes(tpe)

  def typeAttributes[A](tpe: Type[A]): A = tpe.attributes

  def eraseAttributes[A](typeDef: Definition[A]): UDefinition = typeDef.eraseAttributes

  def collectVariables[A](tpe: Type[A]): Set[Name]    = tpe.collectVariables
  def collectReferences[A](tpe: Type[A]): Set[FQName] = tpe.collectReferences
  def substituteTypeVariables[A](mapping: Map[Name, Type[A]], tpe: Type[A]): Type[A] =
    ??? // tpe.substituteTypeVariables

  def toString[A](tpe: Type[A]): String = tpe.toString
}

object TypeModule extends TypeModule
