package com.morganstanley.morphir.ir

case class Path(value: List[Name]) extends AnyVal {
  def mapSegments[A](fn: Name => A): List[A] =
    value.map(fn)
}

object Path {

  val empty: Path = Path(List.empty)

  def apply(head: Name, rest: Name*): Path =
    Path(head :: rest.toList)

  def fromString(str: String): Path = {
    val separatorRegex = """[^\w\s]+""".r
    fromList(separatorRegex.split(str).map(Name.fromString).toList)
  }

  def toString(nameToString: Name => String, sep: String, path: Path): String =
    path.mapSegments(nameToString).mkString(sep)

  def fromList(names: List[Name]): Path =
    Path(names)

  def toList(path: Path): List[Name] = path.value

  def isPrefixOf(prefix: Path, path: Path): Boolean = (prefix, path) match {
    // empty path is a prefix of any other path
    case (Path.empty, _) => true
    // empty path has no prefixes except the empty prefix captured above
    case (_, Path.empty) => false
    case (Path(prefixHead :: prefixTail), Path(pathHead :: pathTail)) =>
      if (prefixHead == pathHead)
        isPrefixOf(Path(prefixTail), Path(pathTail))
      else
        false
  }

}
