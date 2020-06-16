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
