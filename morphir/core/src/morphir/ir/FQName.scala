package morphir.ir

case class FQName(packagePath: Path, modulePath: Path, localName: Name) {}

object FQName {
  val fQName = (packagePath: Path) =>
    (modulePath: Path) =>
      (localName: Name) => FQName(packagePath, modulePath, localName)
}
