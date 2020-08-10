/*
Copyright 2020 Morgan Stanley

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/


package morphir.ir

import morphir.ir.codec.FQNameCodec
import morphir.ir.path.Path

case class FQName(packagePath: Path, modulePath: Path, localName: Name) {
  def toTuple: (Path, Path, Name) =
    (packagePath, modulePath, localName)

  override def toString: String =
    Seq(packagePath.toCamelCase(), modulePath.toCamelCase(), localName.toTitleCase).mkString(".")
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
