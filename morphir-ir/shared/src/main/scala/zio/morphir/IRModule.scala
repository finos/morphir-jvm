package zio.morphir

import zio.Chunk
import zio.morphir.ir.*
import zio.morphir.ir.{TypeModule, ValueModule}
import zio.morphir.ir.TypeModule.UType

object IRModule {

  final case class IR(
      valueSpecifications: Map[FQName, ValueModule.Specification[Any]],
      valueDefinitions: Map[FQName, ValueModule.Definition[UType]],
      typeSpecifications: Map[FQName, TypeModule.Specification[Any]],
      typeConstructors: Map[FQName, (FQName, Chunk[Name], Chunk[(Name, UType)])]
  ) { self => }

  object IR {
    val empty: IR = IR(
      valueSpecifications = Map.empty,
      valueDefinitions = Map.empty,
      typeSpecifications = Map.empty,
      typeConstructors = Map.empty
    )

    def fromDistribution(distribution: DistributionModule.Distribution): IR = ???

    def fromPackageSpecifivations(specs: Map[FQName, PackageModule.Specification[Any]]): IR = ???
  }
}
