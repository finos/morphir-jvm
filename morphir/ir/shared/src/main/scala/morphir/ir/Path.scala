package morphir.ir

import morphir.ir.typeclass.instances.PathInstances

case class Path(value: List[Name]) {
  @inline def toList: List[Name] = value

  def mapSegments[A](fn: Name => A): List[A] =
    value.map(fn)

  def allSegments: List[String] =
    value.flatMap(n => n.value)

  override def toString: String =
    value.mkString(".")
}

object Path extends PathInstances {

  implicit val empty: Path = Path(List.empty)

  def apply(head: Name, rest: Name*): Path =
    Path(head :: rest.toList)

  def fromString(str: String): Path = {
    val separatorRegex = """[^\w\s]+""".r
    fromList(separatorRegex.split(str).toList.map(Name.fromString))
  }

  def toString(nameToString: Name => String, sep: String, path: Path): String =
    path.mapSegments(nameToString).mkString(sep)

  def fromList(names: List[Name]): Path =
    Path(names)

  def fromNames(name: Name, rest: Name*): Path =
    Path(name :: rest.toList)

  def path(name: Name, rest: Name*): Path =
    fromNames(name, rest: _*)

  def toList(path: Path): List[Name] = path.value

  @scala.annotation.tailrec
  def isPrefixOf(prefix: Path, path: Path): Boolean = (prefix, path) match {
    // empty path is a prefix of any other path
    case (Path(Nil), _) => true
    // empty path has no prefixes except the empty prefix captured above
    case (_, Path(Nil)) => false
    case (Path(prefixHead :: prefixTail), Path(pathHead :: pathTail)) =>
      if (prefixHead == pathHead)
        isPrefixOf(Path(prefixTail), Path(pathTail))
      else
        false
  }
}
