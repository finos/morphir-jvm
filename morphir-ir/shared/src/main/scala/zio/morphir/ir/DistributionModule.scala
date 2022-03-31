package zio.morphir.ir

import zio.morphir.ir.types.UType

object DistributionModule {
  sealed trait Distribution
  object Distribution {
    final case class Library(
        packageName: PackageName,
        dependencies: Map[PackageName, UPackageSpecification],
        packageDef: PackageDefinition[UType]
    ) extends Distribution { self =>

      def findModuleSpecification(packageName: PackageName, module: ModuleName): Option[UModuleSpecification] =
        self match {
          case Library(`packageName`, _, packageDef) =>
            packageDef.toSpecification.modules.get(module)
            ???
          case Library(_, _, _) => None
        }
    }
  }

}
