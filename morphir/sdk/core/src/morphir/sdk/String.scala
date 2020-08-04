package morphir.sdk

import morphir.sdk.Maybe.Maybe

object String {
  type String = scala.Predef.String

  @inline def isEmpty(str: String): Boolean = str.isEmpty()

  @inline def length(str: String): Int = str.length()

  @inline def reverse(str: String): String = str.reverse

  def repeat(times: Int, str: String): String = Array.fill[String](times)(str).mkString

  @inline def replace(literal: String, replacement: String, target: String): String =
    target.replace(literal, replacement)

  @inline def fromInt(int: Int): String =
    int.toString

  @inline def append(first: String, second: String): String = first + second

  @inline def ++(first: String, second: String): String = first + second

  @inline def concat(strings: List[String]): String = strings.mkString

  @inline def split(sep: String, target: String): List[String] =
    target.split(sep).toList //TODO: These aren't exactly the same

  def toInt(text: String): Maybe[Int] =
    try {
      Maybe.just(text.toInt)
    } catch {
      case _: NumberFormatException => Maybe.nothing
    }

  @inline def toUpper(text: String): String =
    text.toUpperCase()

  @inline def toLower(text: String): String =
    text.toLowerCase()

  @inline def trim(text: String): String = text.trim()

  def join(sep: Char)(chunks: List[String]): String = chunks.mkString(sep.toString())

  def words(str: String): List[String] = str.split("\\s").toList

  def lines(str: String): List[String] = str.split("\\n").toList

  def slice(start: Int)(end: Int)(str: String): String = str.substring(start, end)

  def left(n: Int)(str: String): String = str.substring(0, n)

  def right(n: Int)(str: String): String = str.slice(str.length - n, str.length)

  def dropLeft(n: Int)(str: String): String = str.drop(n)

  def dropRight(n: Int)(str: String): String = str.dropRight(n)

  def contains(substring: String)(str: String): Boolean = str.contains(substring)

  def startsWith(substring: String)(str: String): Boolean = str.startsWith(substring)

  def endsWith(substring: String)(str: String): Boolean = str.endsWith(substring)

  def indexes(substring: String)(str: String): List[Int] = str.r.findAllMatchIn(substring).map(_.start).toList

  def indices(substring: String)(str: String): List[Int] = indexes(substring)(str)

  def toFloat(str: String): Maybe[Float] =
    try {
      Maybe.just(str.toFloat)
    } catch {
      case _: NumberFormatException => Maybe.nothing
    }

  def fromFloat(float: Float): String = float.toString

  def fromChar(ch: Char): String = ch.toString

  def cons(ch: Char)(str: String): String = s"$ch$str"

  def uncons(str: String): Maybe[(Char, String)] =
    str match {
      case a if a.length == 0 => Maybe.nothing
      case a if a.length == 1 => Maybe.just((a.charAt(0), ""))
      case _                  => Maybe.just((str.charAt(0), str.substring(1, str.length)))
    }

  def toList(str: String): List[Char] = str.toList

  def fromList(chList: List[Char]): String = chList.mkString

  def pad(n: Int)(ch: Char)(str: String): String = str.padTo(n, ch)

  def padLeft(n: Int)(ch: Char)(str: String): String = ???

  def padRight(n: Int)(ch: Char)(str: String): String = ???

  def trimLeft(str: String): String = str.replaceAll("^\\s+", "")

  def trimRight(str: String): String = str.replaceAll("\\s+$", "")

  def map(func: (Char, Char) => String)(str: String): String = ???

  def filter(func: (Char, Boolean) => String)(str: String): String = ???

  def foldl[B](func: (Char, B) => B)(z: B)(str: String): B = ???

  def foldr[B](func: (Char, B) => B)(z: B)(str: String): B = ???

  def any(func: (Char, Boolean) => String)(str: String): Boolean = ???

  def all(func: (Char, Boolean) => String)(str: String): Boolean = ???

  implicit class StringOps(private val self: String) extends AnyVal {
    def ++(that: String): String = self + that
  }
}
