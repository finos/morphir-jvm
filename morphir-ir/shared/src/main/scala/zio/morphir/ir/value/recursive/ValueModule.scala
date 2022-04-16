package zio.morphir.ir.value.recursive

import zio.morphir.ir.value.PatternConstructors
import zio.morphir.ir.{FQName, Name}

trait ValueModule extends ValueConstructors with PatternConstructors {

  import ValueModule.MapValueAttributesPartiallyApplied

  final type Definition[+TA, +VA] = zio.morphir.ir.value.recursive.Definition[TA, VA]
  final val Definition: zio.morphir.ir.value.recursive.Definition.type = zio.morphir.ir.value.recursive.Definition

  final type ValueDefinition[+TA, +VA] = zio.morphir.ir.value.recursive.Definition[TA, VA]
  final val ValueDefinition: zio.morphir.ir.value.recursive.Definition.type = zio.morphir.ir.value.recursive.Definition

  final type Pattern[+A] = zio.morphir.ir.value.Pattern[A]
  final val Pattern: zio.morphir.ir.value.Pattern.type = zio.morphir.ir.value.Pattern

  final type RawValue = zio.morphir.ir.value.recursive.Value.RawValue
  final val RawValue: zio.morphir.ir.value.recursive.Value.type = zio.morphir.ir.value.recursive.Value

  final type Specification[+A] = zio.morphir.ir.value.Specification[A]
  final val Specification: zio.morphir.ir.value.Specification.type = zio.morphir.ir.value.Specification

  final type TypedValue = zio.morphir.ir.value.recursive.Value.TypedValue
  final val TypedValue: zio.morphir.ir.value.recursive.Value.type = zio.morphir.ir.value.recursive.Value.TypedValue

  final type Value[+TA, +VA] = zio.morphir.ir.value.recursive.Value[TA, VA]
  final val Value: zio.morphir.ir.value.recursive.Value.type = zio.morphir.ir.value.recursive.Value

  final def collectReferences[TA, VA](value: Value[TA, VA]): Set[FQName] = value.collectReferences
  final def collectVariables[TA, VA](value: Value[TA, VA]): Set[Name]    = value.collectVariables

  final def mapValueAttributes[TA, TB, VA, VB](f: TA => TB, g: VA => VB, value: Value[TA, VA]): Value[TB, VB] =
    value.mapAttributes(f, g)

  final def mapValueAttributes[TA, VA](value: Value[TA, VA]): MapValueAttributesPartiallyApplied[TA, VA] =
    new MapValueAttributesPartiallyApplied(value)

  final def patternAttribute[A](pattern: Pattern[A]): A = pattern.attributes

  final def toRawValue[TA, VA](value: Value[TA, VA]): RawValue = value.toRawValue

  final def uncurryApply[TA, VA](
      fun: Value[TA, VA],
      lastArg: Value[TA, VA]
  ): (Value[TA, VA], scala.List[Value[TA, VA]]) =
    fun.uncurryApply(lastArg)

  final def valueAttribute[VA](value: Value[Nothing, VA]): VA = value.attributes
}

object ValueModule {
  final class MapValueAttributesPartiallyApplied[TA, VA](val value: Value[TA, VA]) extends AnyVal {
    def apply[TB, VB](f: TA => TB, g: VA => VB): Value[TB, VB] =
      value.mapAttributes(f, g)
  }
}
