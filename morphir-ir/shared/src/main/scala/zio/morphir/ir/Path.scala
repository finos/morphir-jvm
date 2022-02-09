package zio.morphir.ir
import zio.Chunk
import zio.morphir.ir.PackageModule.PackageAndModulePath

import scala.annotation.tailrec

final case class Path(segments: Chunk[Name]) { self =>

  /** Constructs a new path by combining this path with the given name. */
  def /(name: Name): Path = Path(segments ++ Chunk(name))

  /** Constructs a new path by combining this path with the given path. */
  def /(name: Path): Path = Path(segments ++ name.segments)
  def %(other: Path): PackageAndModulePath =
    PackageAndModulePath(PackageName(self), ModulePath(other))

  def %(name: Name): ModuleName = ModuleName(self, name)

  /** Indicates whether this path is empty. */
  def isEmpty: Boolean               = segments.isEmpty
  def zip(other: Path): (Path, Path) = (self, other)

  def toList: List[Name] = segments.toList

  def toString(f: Name => String, separator: String): String =
    segments.map(f).mkString(separator)

  /** Checks if this path is a prefix of provided path */
  def isPrefixOf(path: Path): Boolean = Path.isPrefixOf(self, path)
}

object Path {
  val empty: Path = Path(Chunk.empty)

  private def wrap(value: List[Name]): Path = Path(Chunk.fromIterable(value))

  def fromString(str: String): Path = {
    val separatorRegex = """[^\w\s]+""".r
    wrap(separatorRegex.split(str).map(Name.fromString).toList)
  }

  def toString(f: Name => String, separator: String, path: Path): String =
    path.toString(f, separator)

  @inline def fromList(names: List[Name]): Path = wrap(names)

  @inline def toList(path: Path): List[Name] = path.segments.toList

  /** Checks if the first provided path is a prefix of the second path */
  @tailrec
  def isPrefixOf(prefix: Path, path: Path): Boolean = (prefix.toList, path.toList) match {
    case (Nil, _) => true
    case (_, Nil) => false
    case (prefixHead :: prefixTail, pathHead :: pathTail) =>
      if (prefixHead == pathHead)
        isPrefixOf(
          Path.fromList(prefixTail),
          Path.fromList(pathTail)
        )
      else false
  }
}
