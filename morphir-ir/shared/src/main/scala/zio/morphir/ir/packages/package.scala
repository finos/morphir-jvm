package zio.morphir.ir

package object packages {

  final type USpecification = zio.morphir.ir.packages.Specification[Unit]
  final val USpecification: zio.morphir.ir.packages.Specification.type = zio.morphir.ir.packages.Specification
}
