package zio.morphir.ir.value.recursive

import zio.morphir.ir.value.{Pattern, PatternConstructors}

trait ValueModule extends ValueConstructors with PatternConstructors {

  import ValueModule.MapValueAttributesPartiallyApplied

  final def mapValueAttributes[TA, TB, VA, VB](f: TA => TB, g: VA => VB, value: Value[TA, VA]): Value[TB, VB] =
    value.mapAttributes(f, g)

  final def mapValueAttributes[TA, VA](value: Value[TA, VA]): MapValueAttributesPartiallyApplied[TA, VA] =
    new MapValueAttributesPartiallyApplied(value)

  final def patternAttribute[A](pattern: Pattern[A]): A = pattern.attributes

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
