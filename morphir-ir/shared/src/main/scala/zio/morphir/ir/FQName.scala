package zio.morphir.ir

final case class FQName(packagePath: PackageName, modulePath: ModulePath, localName: Name)

object FQName {
  def apply(packagePath: Path, modulePath: Path, localName: Name): FQName =
    FQName(PackageName(packagePath), ModulePath(modulePath), localName)

  val fqName: Path => Path => Name => FQName = packagePath =>
    modulePath => localName => FQName(PackageName(packagePath), ModulePath(modulePath), localName)

  /** Get the package path part of a fully-qualified name. */
  def getPackagePath(fqName: FQName): Path = fqName.packagePath.toPath
}
