package morphir.ir

import io.circe.{ Decoder, Encoder }

case class FQName(packagePath: Path, modulePath: Path, localName: Name) {
  def toTuple: (Path, Path, Name) =
    (packagePath, modulePath, localName)
}

object FQName {

  implicit val encodeFQName: Encoder[FQName] =
    Encoder.encodeTuple3[Path, Path, Name].contramap(fqn => (fqn.packagePath, fqn.modulePath, fqn.localName))

  implicit val decodeFQName: Decoder[FQName] =
    Decoder.decodeTuple3[Path, Path, Name].map {
      case (packagePath, modulePath, localName) => FQName(packagePath, modulePath, localName)
    }

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
