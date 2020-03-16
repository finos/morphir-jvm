package morphir.ir

import scala.util.matching.Regex
import java.nio.charset.StandardCharsets
import scala.annotation.tailrec

import upickle.default.{readwriter, ReadWriter => RW, macroRW}
import upickle.default._
import zio.test.Gen

case class Name private[ir] (value: List[String]) extends AnyVal {
  override def toString: String = value.mkString("[", ",", "]")

  def jsonEncode: ujson.Value =
    writeJs(this)
}

object Name {
  implicit val readWriter: RW[Name] =
    readwriter[List[String]]
      .bimap[Name](name => name.value, value => Name(value))

  def apply(firstWord: String, otherWords: String*): Name =
    Name(firstWord :: otherWords.toList)

  def fromString(str: String): Name = {
    val pattern = """[a-zA-Z][a-z]*|[0-9]+""".r
    Name(pattern.findAllIn(str).toList.map(_.toLowerCase()))
  }

  def name(head: String, rest: String*): Name =
    Name(head :: (rest.toList))

  implicit def fromList(words: List[String]): Name =
    Name(words)

  def toList(name: Name): List[String] = name.value

  implicit def toTitleCase(name: Name): String =
    toList(name)
      .map(_.capitalize)
      .mkString("")

  implicit def toCamelCase(name: Name): String =
    toList(name) match {
      case Nil => ""
      case head :: tail =>
        (head :: (tail.map(_.capitalize))).mkString("")
    }

  implicit def toSnakeCase(name: Name): String =
    toHumanWords(name).mkString("_")

  implicit def toKebabCase(name: Name): String =
    toHumanWords(name).mkString("-")

  implicit def toHumanWords(name: Name): List[String] = {
    val words = toList(name)
    val join: List[String] => String = abbrev =>
      abbrev.map(_.toUpperCase()).mkString("")

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

  def encodeName(name: Name): ujson.Value =
    name.jsonEncode

  def decodeName(json: ujson.Value) =
    read[Name](json)

  val fuzzName: Gen[zio.random.Random with zio.test.Sized, Name] = {
    val fuzzWord = {
      val choices =
        Seq(
          "area",
          "benchmark",
          "book",
          "business",
          "company",
          "country",
          "currency",
          "day",
          "description",
          "entity",
          "fact",
          "family",
          "from",
          "government",
          "group",
          "home",
          "id",
          "job",
          "left",
          "lot",
          "market",
          "minute",
          "money",
          "month",
          "name",
          "number",
          "owner",
          "parent",
          "part",
          "problem",
          "rate",
          "right",
          "state",
          "source",
          "system",
          "time",
          "title",
          "to",
          "valid",
          "week",
          "work",
          "world",
          "year"
        ).map(Gen.const(_))
      Gen.oneOf(choices: _*)
    }

    Gen.listOf(fuzzWord).map(lst => lst.take(3)).map(fromList(_))
  }
}
