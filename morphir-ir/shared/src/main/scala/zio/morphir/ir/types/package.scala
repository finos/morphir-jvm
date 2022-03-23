package zio.morphir.ir

package object types {

  final type UConstructors = Constructors[Unit]
  final val UConstructors: Constructors.type = types.Constructors

  final type UDefinition = Definition[Unit]
  final val UDefinition: Definition.type = types.Definition

  final type UField = Field[Unit]
  final val UField: Field.type = types.Field

  final type USpecification = Specification[Unit]
  final val USpecification: Specification.type = types.Specification

  /** Represents an un-annotated/un-attributed type. */
  type UType = Type[Unit]
  val UType: Type.type = Type

}
