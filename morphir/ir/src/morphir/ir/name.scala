package morphir.ir

import morphir.ir.codec.NameCodec
import morphir.ir.path.Path

import scala.annotation.tailrec

object name {

  def apply(head: String, rest: String*): Name = Name.name(head, rest: _*)

  final case class Name private (value: List[String]) extends AnyVal {

    def ::(segment: String): Name =
      Name(segment :: value)

    def ++(other: Name): Name = Name(value ++ other.value)

    def /(other: Name): Path = Path(this, other)

    def mapSegments(f: String => String): Name =
      Name(value.map(f))

    def mkString(f: String => String)(sep: String): String =
      value.map(f).mkString(sep)

    @inline def segments: List[String] = value

    @inline def toList: List[String] = value

    override def toString: String = toTitleCase

    def toLowerCase: String =
      mkString(part => part.toLowerCase)("")

    def toCamelCase: String =
      value match {
        case Nil => ""
        case head :: tail =>
          (head :: tail.map(_.capitalize)).mkString("")
      }

    def toKebabCase: String =
      humanize.mkString("-")

    def toSnakeCase: String =
      humanize.mkString("_")

    def toTitleCase: String =
      value
        .map(_.capitalize)
        .mkString("")

    def humanize: List[String] = {
      val words                        = value
      val join: List[String] => String = abbrev => abbrev.map(_.toUpperCase()).mkString("")

      @tailrec
      def process(
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
              process(prefix, abbrev ++ List(first), rest)
            else
              abbrev match {
                case Nil => process(prefix ++ List(first), List.empty, rest)
                case _ =>
                  process(prefix ++ List(join(abbrev), first), List.empty, rest)
              }
        }

      process(List.empty, List.empty, words)
    }

    def show: String = value.map(segment => s""""$segment"""").mkString("[", ",", "]")
  }

  object Name extends NameCodec {

    def apply(firstWord: String, otherWords: String*): Name =
      (firstWord :: otherWords.toList).map(fromString).reduce(_ ++ _)

    def fromString(str: String): Name = {
      val pattern = """[a-zA-Z][a-z]*|[0-9]+""".r
      Name(pattern.findAllIn(str).toList.map(_.toLowerCase()))
    }

    def name(head: String, rest: String*): Name =
      Name(head :: rest.toList)

    def fromList(words: List[String]): Name =
      Name(words)

    def toList(name: Name): List[String] = name.value

    @inline def toTitleCase(name: Name): String = name.toTitleCase

    @inline def toCamelCase(name: Name): String = name.toCamelCase

    @inline def toSnakeCase(name: Name): String = name.toSnakeCase

    @inline def toKebabCase(name: Name): String = name.toKebabCase

    @inline def toHumanWords(name: Name): List[String] = name.humanize
  }

}
