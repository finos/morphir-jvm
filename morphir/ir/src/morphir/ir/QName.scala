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

import morphir.ir.codec.QNameCodec
import morphir.ir.path.Path

case class QName(modulePath: Path, localName: Name) {
  def toTuple: (Path, Name) = (modulePath, localName)

  override def toString: String = s"${modulePath}.$localName"
}

object QName extends QNameCodec {
  @inline def toTuple(qname: QName): (Path, Name) = qname.toTuple
  def fromTuple(tuple: (Path, Name)): QName       = QName(tuple._1, tuple._2)
  @inline def getModulePath(qname: QName): Path   = qname.modulePath
  @inline def getLocalName(qname: QName): Name    = qname.localName

  def qName(modulePath: Path, localName: Name): QName =
    QName(modulePath, localName)

  def toString(
    pathPartToString: Name => String,
    nameToString: Name => String,
    sep: String,
    qualifiedName: QName
  ): String =
    (qualifiedName.modulePath.toList.map(pathPartToString) ++ nameToString(
      qualifiedName.localName
    )).mkString(sep)

}
