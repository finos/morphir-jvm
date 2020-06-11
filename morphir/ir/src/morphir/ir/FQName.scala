package morphir.ir

import morphir.ir.codec.FQNameCodec
import morphir.ir.path.Path

case class FQName(packagePath: Path, modulePath: Path, localName: Name) {
  def toTuple: (Path, Path, Name) =
    (packagePath, modulePath, localName)

}

object FQName extends FQNameCodec {

  def fQName(packagePath: Path) =
    (modulePath: Path) => (localName: Name) => FQName(packagePath, modulePath, localName)

  def fQName(packagePath: Path, modulePath: Path, localName: Name) =
    FQName(packagePath, modulePath, localName)

  def fromQName(packagePath: Path, qName: QName): FQName =
    FQName(packagePath, qName.modulePath, qName.localName)

  def getPackagePath(self: FQName): Path = self.packagePath
  def getModulePath(self: FQName): Path  = self.modulePath
  def getLocalName(self: FQName): Name   = self.localName

  def toTuple(self: FQName): (Path, Path, Name) =
    self.toTuple

  def fromTuple(value: (Path, Path, Name)): FQName =
    FQName(value._1, value._2, value._3)

}
