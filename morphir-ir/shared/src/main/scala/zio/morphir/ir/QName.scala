package zio.morphir.ir

final case class QName(modulePath: Path, localName: Name) {
  @inline def toTuple: (Path, Name) = (modulePath, localName)

  override def toString: String =
    if (modulePath.isEmpty) localName.toString
    else modulePath.toString + "." + localName.toString
}

object QName {
  def fromTuple(tuple: (Path, Name)): QName = QName(tuple._1, tuple._2)

  def getLocalName(qname: QName): Name  = qname.localName
  def getModulePath(qname: QName): Path = qname.modulePath
}
