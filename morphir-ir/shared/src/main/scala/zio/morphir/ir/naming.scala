package zio.morphir.ir

import scala.annotation.tailrec
import zio.Chunk

object naming {
  final case class Name private (toList: List[String]) extends AnyVal { self =>
    def :+(that: String): Name = Name(self.toList :+ that)
    def +:(that: String): Name = Name(that +: self.toList)
    def ++(that: Name): Name   = Name(self.toList ++ that.toList)
    def /(that: Name): Path    = Path(Chunk(self, that))

    def humanize: List[String] = {
      val words                        = toList
      val join: List[String] => String = abbrev => abbrev.map(_.toUpperCase()).mkString("")

      @tailrec
      def loop(
          prefix: List[String],
          abbrev: List[String],
          suffix: List[String]
      ): List[String] =
        suffix match {
          case Nil =>
            abbrev match {
              case Nil => prefix
              case _   => prefix ++ List(join(abbrev))
            }
          case first :: rest =>
            if (first.length() == 1)
              loop(prefix, abbrev ++ List(first), rest)
            else
              abbrev match {
                case Nil => loop(prefix ++ List(first), List.empty, rest)
                case _ =>
                  loop(prefix ++ List(join(abbrev), first), List.empty, rest)
              }
        }

      loop(List.empty, List.empty, words.toList)
    }

    /**
     * Maps segments of the `Name`.
     */
    def mapParts(f: String => String): Name = Name(self.toList.map(f))

    def mkString(f: String => String)(sep: String): String =
      toList.map(f).mkString(sep)

    def toLowerCase: String =
      mkString(part => part.toLowerCase)("")

    def toCamelCase: String =
      toList match {
        case Nil => ""
        case head :: tail =>
          (head :: tail.map(_.capitalize)).mkString("")
      }

    def toKebabCase: String =
      humanize.mkString("-")

    def toSnakeCase: String =
      humanize.mkString("_")

    def toTitleCase: String =
      toList
        .map(_.capitalize)
        .mkString("")

  }
  object Name {

    private def wrap(value: List[String]): Name = Name(value)

    def apply(first: String, rest: String*): Name =
      fromIterable(first +: rest)

    @inline def fromList(list: List[String]): Name = wrap(list)
    def fromIterable(iterable: Iterable[String]): Name =
      wrap(iterable.toList)

    /**
     * Creates a new name from a chunk of strings without checking.
     */
    private[ir] def unsafeMake(value: List[String]): Name = Name(value)

    def toList(name: Name): List[String] = name.toList

    @inline def toTitleCase(name: Name): String = name.toTitleCase

    @inline def toCamelCase(name: Name): String = name.toCamelCase

    @inline def toSnakeCase(name: Name): String = name.toSnakeCase

    @inline def toKebabCase(name: Name): String = name.toKebabCase

    @inline def toHumanWords(name: Name): List[String] = name.humanize

  }

  final case class Path(segments: Chunk[Name]) extends AnyVal { self =>
    def /(name: Name): Path = Path(segments ++ Chunk(name))
    def /(name: Path): Path = Path(segments ++ name.segments)
    def %(other: Path): PackageAndModulePath =
      PackageAndModulePath(PackageName(self), ModulePath(other))
    def %(name: Name): ModuleName      = ModuleName(self, name)
    def zip(other: Path): (Path, Path) = (self, other)
  }

  final case class PackageName(toPath: Path) { self =>
    def %(modulePath: ModulePath): PackageAndModulePath = PackageAndModulePath(self, modulePath)
    def %(moduleName: ModuleName): FQName = FQName(self, ModulePath(moduleName.namespace), moduleName.localName)
  }
  final case class ModulePath(toPath: Path)

  final case class ModuleName(namespace: Path, localName: Name) {
    lazy val toPath = namespace / localName
  }

  final case class PackageAndModulePath(packageName: PackageName, modulePath: ModulePath) { self =>
    def %(name: Name): FQName = FQName(packageName, modulePath, name)
  }
  final case class FQName(packagePath: PackageName, modulePath: ModulePath, localName: Name)
}
