package zio.morphir.ir.value

import zio.morphir.ir.{FQName, Name}
import zio.morphir.ir.value

trait ValueModule extends ValueSyntax {

  final type Definition[+TA, +VA] = value.Definition[TA, VA]
  val Definition: value.Definition.type = value.Definition

  final type Pattern[+VA] = value.Pattern[VA]
  val Pattern: value.Pattern.type = value.Pattern

  final type RawValue = zio.morphir.ir.value.RawValue
  val RawValue: zio.morphir.ir.value.RawValue.type = zio.morphir.ir.value.RawValue

  final type Specification[+TA] = value.Specification[TA]
  val Specification: value.Specification.type = value.Specification

  final type TypedValue = zio.morphir.ir.value.TypedValue
  val TypedValue: zio.morphir.ir.value.TypedValue.type = zio.morphir.ir.value.TypedValue

  final type Value[+TA, +VA] = zio.morphir.ir.value.Value[TA, VA]
  val Value: zio.morphir.ir.value.Value.type = zio.morphir.ir.value.Value

  def toRawValue[TA, VA](value: Value[TA, VA]): RawValue = value.toRawValue

  final def collectVariables[TA, VA](value: Value[TA, VA]): Set[Name] = value.collectVariables

  final def collectReferences[TA, VA](value: Value[TA, VA]): Set[FQName] = value.collectReferences

  def definitionToSpecification[TA, VA](definition: Definition[TA, VA]): Specification[TA] =
    definition.toSpecification

  def definitionToValue[TA, VA](definition: Definition[TA, VA]): Value[TA, VA] =
    definition.toValue

  def valuesAttribute[TA, VA](value: Value[TA, VA]): VA = value.attributes
}

object ValueModule extends ValueModule
