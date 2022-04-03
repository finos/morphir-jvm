package zio.morphir.ir.packages

trait PackageModule {

  final type Definition[+TA, +VA] = zio.morphir.ir.packages.Definition[TA, VA]
  final val Definition: zio.morphir.ir.packages.Definition.type = zio.morphir.ir.packages.Definition

  final type PackageAndModulePath = zio.morphir.ir.packages.PackageAndModulePath
  final val PackageAndModulePath: zio.morphir.ir.packages.PackageAndModulePath.type =
    zio.morphir.ir.packages.PackageAndModulePath

  final type PackageName = zio.morphir.ir.packages.PackageName
  final val PackageName: zio.morphir.ir.packages.PackageName.type = zio.morphir.ir.packages.PackageName

  final type PackageSpecFor[A] = zio.morphir.ir.packages.PackageSpecFor[A]
  final val PackageSpecFor: zio.morphir.ir.packages.PackageSpecFor.type = zio.morphir.ir.packages.PackageSpecFor

  final type Specification[+TA] = zio.morphir.ir.packages.Specification[TA]
  final val Specification: zio.morphir.ir.packages.Specification.type = zio.morphir.ir.packages.Specification

  final type USpecification = zio.morphir.ir.packages.Specification[Unit]
  final val USpecification: zio.morphir.ir.packages.Specification.type = zio.morphir.ir.packages.Specification

  val emptySpecification: Specification[Nothing] = Specification.empty
}

object PackageModule extends PackageModule
