package zio.morphir.ir.types

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

  final def definitionToSpecification[A](definition: Definition[A]): Specification[A] =
    definition.toSpecification

  final def mapSpecificationAttributes[A, B](f: A => B, spec: Specification[A]): Specification[B] = ???
//    spec.map(f)

//  def mapDefinitionAttributes[A, B] (f: A => B, defn: Definition[A]) : Definition[B] = defn.map(f)

}

final object TypeModule extends TypeModule
