package morphir.ir
import upickle.default.{readwriter, ReadWriter => RW, macroRW}
import upickle.default._

case class QName(modulePath: Path, localName: Name) {
  def toTuple: (Path, Name) = modulePath -> localName
}

object QName {
  implicit val readWriter: RW[QName] =
    readwriter[(Path, Name)].bimap[QName](
      toTuple,
      fromTuple
    )

  @inline def toTuple(qname: QName): (Path, Name) = qname.toTuple
  def fromTuple(tuple: (Path, Name)): QName = QName(tuple._1, tuple._2)
  @inline def getModulePath(qname: QName): Path = qname.modulePath
  @inline def getLocalName(qname: QName): Name = qname.localName

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

  //def encodeQName(self:QName):ujson.Value = ???

}
