package org.morphir.sdk

import org.morphir.sdk.Maybe.Maybe

object String {
  type String = ScalaString

  @inline def isEmpty(str: String): Boolean = str.isEmpty()

  @inline def length(str: String): Int = str.length()

  @inline def reverse(str: String): String = str.reverse

  def repeat(times: Int, str: String): String = Array.fill[String](times)(str).mkString

  @inline def replace(literal: String, replacement: String, target: String): String =
    target.replaceAllLiterally(literal, replacement)

  @inline def fromInt(int: Int): String =
    int.toString

  @inline def append(first: String, second: String): String = first + second

  @inline def ++(first: String, second: String): String = first + second

  @inline def concat(strings: List[String]): String = strings.mkString

  @inline def split(sep: String, target: String): String = target.split(sep) //TODO: These aren't exactly the same

  @inline def toInt(text: String): Maybe[Int] =
    StringOps.toInt(text)

  @inline def toUpper(text: String): String =
    text.toUpperCase()

  @inline def toLower(text: String): String =
    text.toLowerCase()

  @inline def trim(text: String): String = text.trim()

  implicit class StringOps(val self: String) extends AnyVal {
    def ++(that: String): String = self + that
  }
}
