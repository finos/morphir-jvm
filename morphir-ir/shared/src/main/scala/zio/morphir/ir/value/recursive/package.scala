package zio.morphir.ir.value

package object recursive {
  type RawValue = zio.morphir.ir.value.recursive.Value.RawValue
  val RawValue: zio.morphir.ir.value.recursive.Value.type = zio.morphir.ir.value.recursive.Value.RawValue

  type TypedValue = zio.morphir.ir.value.recursive.Value.TypedValue
  val TypedValue: zio.morphir.ir.value.recursive.Value.type = zio.morphir.ir.value.recursive.Value.TypedValue
}
