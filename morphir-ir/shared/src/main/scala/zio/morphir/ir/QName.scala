package zio.morphir.ir

final case class QName(modulePath: Path, localName: Name) {
  @inline def toTuple: (Path, Name) = (modulePath, localName)

  override def toString: String =
    modulePath.toString(Name.toTitleCase, ".") + ":" + localName.toCamelCase

}

object QName {
  def toTuple(qName: QName): (Path, Name)   = qName.toTuple
  def fromTuple(tuple: (Path, Name)): QName = QName(tuple._1, tuple._2)

  def fromName(modulePath: Path, localName: Name): QName = QName(modulePath, localName)

  def getLocalName(qname: QName): Name  = qname.localName
  def getModulePath(qname: QName): Path = qname.modulePath

  def toString(qName: QName): String = qName.toString

  def fromString(str: String): Option[QName] =
    str.split(":") match {
      case Array(packageNameString, localNameString) =>
        Some(QName(Path.fromString(packageNameString), Name.fromString(localNameString)))
      case _ => None
    }
}
