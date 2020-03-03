package com.morganstanley.morphir.ir

import scala.util.matching.Regex
import java.nio.charset.StandardCharsets

case class Name private[ir] (value: List[String]) extends AnyVal

object Name {
  def fromString(str: String): Name = {
    val pattern = """[a-zA-Z][a-z]*|[0-9]+""".r
    Name(pattern.findAllIn(str).toList.map(_.toLowerCase()))
  }

  def fromList(words: List[String]): Name =
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
}
