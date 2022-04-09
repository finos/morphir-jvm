package zio.morphir.ir.types

package object nonrecursive {

  final type UConstructors = Constructors[Any]
  final val UConstructors: Constructors.type = nonrecursive.Constructors

  final type UDefinition = Definition[Any]
  final val UDefinition: Definition.type = nonrecursive.Definition

  final type UField = Field[Any]
  final val UField: Field.type = nonrecursive.Field

  final type USpecification = Specification[Any]
  final val USpecification: Specification.type = nonrecursive.Specification

  /** Represents an un-annotated/un-attributed type. */
  type UType = Type[Any]
  val UType: Type.type = Type
}
