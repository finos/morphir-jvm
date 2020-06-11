package morphir.ir.sdk

import morphir.ir.module.ModulePath
import morphir.ir.name.Name
import morphir.ir.path.Path
import morphir.ir.MorphirPackage.PackagePath
import morphir.ir.{ FQName, QName }

object Common {
  def packageName: PackagePath =
    PackagePath(Path.fromString("Morphir.SDK"))

  def toFQName(modulePath: ModulePath, localName: scala.Predef.String): FQName =
    FQName.fromQName(packageName.toPath, QName.qName(modulePath, Name.fromString(localName)))
}
