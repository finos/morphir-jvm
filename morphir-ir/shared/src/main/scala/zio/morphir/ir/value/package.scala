package zio.morphir.ir

package object value {

  type RawValue = Value[Unit, Unit]
  val RawValue: Value.type = Value

  type TypedValue = Value[Unit, UType]
  val TypedValue: Value.type = Value

  type USpecification = Value[Unit, Unit]
  val USpecification: Specification.type = Specification

}
