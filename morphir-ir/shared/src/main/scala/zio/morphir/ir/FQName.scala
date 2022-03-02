package zio.morphir.ir

final case class FQName(packagePath: PackageName, modulePath: ModulePath, localName: Name) {
  def getPackagePath: Path = packagePath.toPath
  def getModulePath: Path  = modulePath.toPath

  override def toString: String = Array(
    Path.toString(Name.toTitleCase, ".", packagePath.toPath),
    Path.toString(Name.toTitleCase, ".", modulePath.toPath),
    Name.toCamelCase(localName)
  ).mkString(":")
}

object FQName {
  def apply(packagePath: Path, modulePath: Path, localName: Name): FQName =
    FQName(PackageName(packagePath), ModulePath(modulePath), localName)

  val fqName: Path => Path => Name => FQName = packagePath =>
    modulePath => localName => FQName(PackageName(packagePath), ModulePath(modulePath), localName)

  def fromQName(packagePath: Path, qName: QName): FQName =
    FQName(packagePath, QName.getModulePath(qName), QName.getLocalName(qName))

  /** Get the package path part of a fully-qualified name. */
  def getPackagePath(fqName: FQName): Path = fqName.getPackagePath

  /** Get the module path part of a fully-qualified name */
  def getModulePath(fqName: FQName): Path = fqName.getModulePath

  /** Get the local name part of a fully-qualified name */
  def getLocalName(fqName: FQName): Name = fqName.localName

  /** Convenience function to create a fully-qualified name from 3 strings */
  def fqn(packageName: String, moduleName: String, localName: String): FQName = {
    FQName(Path.fromString(packageName), Path.fromString(moduleName), Name.fromString(localName))
  }

  def toString(fqName: FQName): String = fqName.toString

  /** Parse a string into a FQName using splitter as the separator between package, module, and local names */
  def fromString(fqNameString: String, splitter: String): FQName = {
    fqNameString.split(splitter) match {
      case Array(moduleNameString, packageNameString, localNameString) =>
        fqn(moduleNameString, packageNameString, localNameString)
      case Array(localNameString) =>
        fqn("", "", localNameString)
      case _ => FQName(Path.empty, Path.empty, Name.empty)
    }
  }

  def fromString(fqNameString: String): FQName = fromString(fqNameString, ":")

}
