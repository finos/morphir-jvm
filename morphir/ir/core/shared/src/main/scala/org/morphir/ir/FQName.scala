package org.morphir.ir

import upickle.default.{readwriter, ReadWriter => RW}
import upickle.default._

case class FQName(packagePath: Path, modulePath: Path, localName: Name) {
  def toTuple: (Path, Path, Name) =
    (packagePath, modulePath, localName)
}

object FQName {
  implicit val readWriter: RW[FQName] =
    readwriter[(Path, Path, Name)].bimap[FQName](
      toTuple,
      fromTuple
    )
  def fQName(packagePath: Path) =
    (modulePath: Path) =>
      (localName: Name) => FQName(packagePath, modulePath, localName)

  def fQName(packagePath: Path, modulePath: Path, localName: Name) =
    FQName(packagePath, modulePath, localName)

  def getPackagePath(self: FQName): Path = self.packagePath
  def getModulePath(self: FQName): Path = self.modulePath
  def getLocalName(self: FQName): Name = self.localName

  def toTuple(self: FQName): (Path, Path, Name) =
    self.toTuple

  def fromTuple(value: (Path, Path, Name)): FQName =
    FQName(value._1, value._2, value._3)

  def encodeFQName(instance: FQName): ujson.Value =
    writeJs(instance)

  def decodeFQName(json: ujson.Value): FQName =
    read[FQName](json)

}
