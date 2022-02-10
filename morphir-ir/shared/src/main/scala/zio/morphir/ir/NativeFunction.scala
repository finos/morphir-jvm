package zio.morphir.ir

// def increment(x: Int): Int = x + 1

sealed trait NativeFunction {}
object NativeFunction {
  case object Addition    extends NativeFunction
  case object Subtraction extends NativeFunction
  // TODO: Multiply
  // TODO: All Native functions here: https://github.com/finos/morphir-elm/blob/15.0.0/src/Morphir/IR/SDK/Basics.elm

}
