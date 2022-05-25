package zio.morphir.ir

import Module.{ModulePath, ModuleName}

final case class FQName(packagePath: PackageName, modulePath: ModulePath, localName: Name) {
  def getPackagePath: Path = packagePath.toPath
  def getModulePath: Path  = modulePath.toPath

  def getModuleName: ModuleName = modulePath.toModuleName

  def toReferenceName: String = Seq(
    Path.toString(Name.toTitleCase, ".", packagePath.toPath),
    Path.toString(Name.toTitleCase, ".", modulePath.toPath),
    localName.toTitleCase
  ).mkString(".")

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

  def fromQName(qName: QName)(implicit options: FQNamingOptions): FQName =
    FQName(options.defaultPackage, ModulePath(QName.getModulePath(qName)), QName.getLocalName(qName))

  /** Get the package path part of a fully-qualified name. */
  def getPackagePath(fqName: FQName): Path = fqName.getPackagePath

  /** Get the module path part of a fully-qualified name */
  def getModulePath(fqName: FQName): Path = fqName.getModulePath

  /** Get the local name part of a fully-qualified name */
  def getLocalName(fqName: FQName): Name = fqName.localName

  /** Convenience function to create a fully-qualified name from 3 strings */
  def fqn(packageName: String, moduleName: String, localName: String): FQName =
    FQName(Path.fromString(packageName), Path.fromString(moduleName), Name.fromString(localName))

  /** Convenience function to create a fully-qualified name from 2 strings with default package name */
  def fqn(moduleName: String, localName: String)(implicit options: FQNamingOptions): FQName =
    FQName(options.defaultPackage, ModulePath(Path.fromString(moduleName)), Name.fromString(localName))

  /** Convenience function to create a fully-qualified name from 1 string with defaults for package and module */
  def fqn(localName: String)(implicit options: FQNamingOptions): FQName =
    FQName(options.defaultPackage, options.defaultModule, Name.fromString(localName))

  def toString(fqName: FQName): String = fqName.toString

  /** Parse a string into a FQName using splitter as the separator between package, module, and local names */
  def fromString(fqNameString: String, splitter: String)(implicit options: FQNamingOptions): FQName =
    fqNameString.split(splitter) match {
      case Array(packageNameString, moduleNameString, localNameString) =>
        fqn(packageNameString, moduleNameString, localNameString)
      case Array(moduleNameString, localNameString) =>
        fqn(moduleNameString, localNameString)
      case Array(localNameString) =>
        fqn(localNameString)
      case _ => throw ParserError(s"Unable to parse: [$fqNameString] into a valid FQName")
    }

  def fromString(fqNameString: String)(implicit options: FQNamingOptions): FQName =
    fromString(fqNameString, options.defaultSeparator)
}

final case class FQNamingOptions(defaultPackage: PackageName, defaultModule: ModulePath, defaultSeparator: String)

object FQNamingOptions {
  implicit val default: FQNamingOptions = FQNamingOptions(PackageName(Path.empty), ModulePath(Path.empty), ":")
}
