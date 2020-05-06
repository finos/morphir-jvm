package morphir.ir

import morphir.core.newtype.Subtype

import scala.annotation.tailrec

object Name extends Subtype[List[String]] {

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

  def toList(name: Name): List[String] = name

  def toTitleCase(name: Name): String =
    toList(name)
      .map(_.capitalize)
      .mkString("")

  def toCamelCase(name: Name): String =
    toList(name) match {
      case Nil => ""
      case head :: tail =>
        (head :: tail.map(_.capitalize)).mkString("")
    }

  def toSnakeCase(name: Name): String =
    toHumanWords(name).mkString("_")

  def toKebabCase(name: Name): String =
    toHumanWords(name).mkString("-")

  def toHumanWords(name: Name): List[String] = {
    val words                        = toList(name)
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
