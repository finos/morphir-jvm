package zio.morphir.ir.value

import zio.morphir.ir.Type.UType

trait ValueAspect[+LowerTA, -UpperTA, +LowerVA, -UpperVA] {
  def apply[TA >: LowerTA <: UpperTA, VA >: LowerVA <: UpperVA](value: Value[TA, VA]): Value[TA, VA]
}

object ValueAspect {
//  type ValueAspectPoly = ValueAspect[Nothing, Any, Nothing, Any]
//  type ValueAspectAtLeastVA[VA] = ValueAspect[Nothing, Any, Nothing, VA]

  def typed(tpe: UType): ValueAspect[Nothing, Any, UType, Any] =
    new ValueAspect[Nothing, Any, UType, Any] {
      override def apply[TA, VA >: UType](value: Value[TA, VA]): Value[TA, VA] = {
        val _ = tpe
        ???
      }
    }

  // ZIO[Any, Nothing, Int] => ZIO[Clock, Nothing, Int]
  // ZIO[Unit, Nothing, Int] => ZIO[String, Nothing, Int] // can't do it with this aspect encoding
}

// Variable((), Name(a)):RawValue (Value[Unit,Unit])  @@ typed(Reference("Int")) | => TypedValue (Value[Unit, Type[Unit]])
