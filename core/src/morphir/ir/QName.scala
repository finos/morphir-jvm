package morphir.ir

case class QName(modulePath: Path, localName: Name) {
  def toTuple: (Path, Name) = modulePath -> localName
}

object QName {
  @inline def toTuple(qname: QName): (Path, Name) = qname.toTuple
  def fromTuple(path: Path, name: Name): QName = QName(path, name)
  @inline def getModulePath(qname: QName): Path = qname.modulePath
  @inline def getLocalName(qname: QName): Name = qname.localName

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
