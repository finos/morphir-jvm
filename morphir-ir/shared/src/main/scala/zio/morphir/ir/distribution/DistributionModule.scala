package zio.morphir.ir.distribution

trait DistributionModule {
  final type Distribution = zio.morphir.ir.distribution.Distribution
  final val Distribution: zio.morphir.ir.distribution.Distribution.type = zio.morphir.ir.distribution.Distribution
}

object DistributionModule extends DistributionModule
