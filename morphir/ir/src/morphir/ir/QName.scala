package morphir.ir

import cats.Show
import cats.implicits._
import morphir.ir.codec.QNameCodec

case class QName(modulePath: Path, localName: Name) {
  def toTuple: (Path, Name) = modulePath -> localName

  //override def toString: String = mod
}

object QName extends QNameCodec {

  implicit val show: Show[morphir.ir.QName] =
    Show.show[morphir.ir.QName](qn => show"${qn.modulePath}.${qn.localName}")

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
