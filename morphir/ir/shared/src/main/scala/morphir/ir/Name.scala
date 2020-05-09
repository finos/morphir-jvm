package morphir.ir

import morphir.ir.codec.NameCodec
import morphir.ir.typeclass.instances.NameInstances

import scala.annotation.tailrec

final case class Name(value: List[String]) extends AnyVal {
  def ::(segment: String): Name =
    Name(segment :: value)

  def mapSegments(f: String => String): Name =
    Name(value.map(f))

  override def toString: String = toKebabCase

  def toCamelCase: String =
    value match {
      case Nil => ""
      case head :: tail =>
        (head :: tail.map(_.capitalize)).mkString("")
    }

  def toKebabCase: String =
    toHumanWords.mkString("-")

  def toSnakeCase: String =
    toHumanWords.mkString("_")

  def toTitleCase: String =
    value
      .map(_.capitalize)
      .mkString("")

  def toHumanWords: List[String] = {
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
}

object Name extends NameCodec with NameInstances {

  def apply(firstWord: String, otherWords: String*): Name =
    Name(firstWord :: otherWords.toList)

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

  @inline def toHumanWords(name: Name): List[String] = name.toHumanWords
}
