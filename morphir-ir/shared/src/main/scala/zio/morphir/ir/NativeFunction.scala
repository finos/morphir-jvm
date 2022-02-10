package zio.morphir.ir

sealed trait NativeFunction {}
object NativeFunction {
  case object Addition extends NativeFunction
}
