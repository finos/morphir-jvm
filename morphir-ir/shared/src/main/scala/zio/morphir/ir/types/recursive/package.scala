package zio.morphir.ir.types

package object recursive {

  type Result[+E, +A] = Either[E, A]
  val Result: Either.type = Either

  type UType = zio.morphir.ir.types.recursive.Type.UType
  val UType: zio.morphir.ir.types.recursive.Type.type = zio.morphir.ir.types.recursive.Type.UType
}
