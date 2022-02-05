package zio.morphir.ir
object PackageModule {
  type Definition[+Annotations] = MorphirIR.PackageDefinition[Annotations]
  val Definition = MorphirIR.PackageDefinition

  type Specification[+Annotations] = MorphirIR.PackageSpecification[Annotations]
  val Specification = MorphirIR.PackageSpecification

  final case class PackageName(toPath: Path) { self =>
    def %(modulePath: ModulePath): PackageAndModulePath = PackageAndModulePath(self, modulePath)
    def %(moduleName: ModuleName): FQName =
      FQName(self, ModulePath(moduleName.namespace), moduleName.localName)
  }

  final case class PackageAndModulePath(packageName: PackageName, modulePath: ModulePath) { self =>
    def %(name: Name): FQName = FQName(packageName, modulePath, name)
  }
}

trait PackageSpecFor[A] {
  import PackageModule.*

  def packageName: PackageName
  def spec: Specification[Any]
  def nativeFunctions: Map[FQName, NativeFunction]
}

object PackageSpecFor {
  def apply[A](implicit packageSpecFor: PackageSpecFor[A]): PackageSpecFor[A] = packageSpecFor
}
