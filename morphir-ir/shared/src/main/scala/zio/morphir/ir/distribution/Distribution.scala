package zio.morphir.ir.distribution
import zio.morphir.ir.PackageModule.{
  Definition => PackageDefinition,
  PackageName,
  USpecification => UPackageSpecification
}
import zio.morphir.ir.module.{ModuleName, Specification}

sealed trait Distribution
object Distribution {
  final case class Library(
      packageName: PackageName,
      dependencies: Map[PackageName, UPackageSpecification],
      packageDef: PackageDefinition.Typed
  ) extends Distribution { self =>

    def findModuleSpecification(packageName: PackageName, module: ModuleName): Option[Specification.Raw] =
      self match {
        case Library(`packageName`, _, packageDef) =>
          packageDef.toSpecification.modules.get(module)
          ???
        case Library(_, _, _) => None
      }
  }
}
