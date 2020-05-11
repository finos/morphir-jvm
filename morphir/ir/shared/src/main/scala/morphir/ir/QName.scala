package morphir.ir

import morphir.ir.typeclass.instances.QNameInstances

case class QName(modulePath: Path, localName: Name) {
  def toTuple: (Path, Name) = modulePath -> localName

  //override def toString: String = mod
}

object QName extends QNameInstances {

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
    qname: QName
  ) =
    (qname.modulePath.toList.map(pathPartToString) ++ nameToString(
      qname.localName
    )).mkString(sep)

}
