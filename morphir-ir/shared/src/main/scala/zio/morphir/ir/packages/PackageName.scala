package zio.morphir.ir.packages
import zio.morphir.ir.{FQName, Path}
import zio.morphir.ir.module._

final case class PackageName(toPath: Path) { self =>
  def %(modulePath: ModulePath): PackageAndModulePath = PackageAndModulePath(self, modulePath)
  def %(moduleName: ModuleName): FQName =
    FQName(self, ModulePath(moduleName.namespace), moduleName.localName)
}

object PackageName {
  def fromString(input: String): PackageName = PackageName(Path.fromString(input))
}
