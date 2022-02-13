package zio.morphir.sexpr.internal

import scala.annotation._

// Fix escaping in a string
final class StringEscape(in: String) {
  private[this] var escaped = false
  private[this] var pos     = 0

  def run: Either[String, String] = {
    val len = in.length
    pos = 0
    val result = new StringBuilder
    try {
      while (pos < len) {
        result += readChar()
      }
      Right(result.toString)
    } catch {
      case e: Exception => Left(s"[$in] -- has an error in string encoding: ${e.getMessage()}")
    }
  }

  def nextChar: Char = {
    pos += 1
    in.charAt(pos - 1)
  }

  def read(): Int = {
    val c = nextChar
    if (escaped) {
      escaped = false
      (c: @switch) match {
        case '"' | '\\' | '/' => c.toInt
        case 'b'              => '\b'.toInt
        case 'f'              => '\f'.toInt
        case 'n'              => '\n'.toInt
        case 'r'              => '\r'.toInt
        case 't'              => '\t'.toInt
        case 'u'              => nextHex4()
        case _ =>
          throw new Exception(s"invalid '\\${c.toChar}' in string")
      }
    } else if (c == '\\') {
      escaped = true
      read()
    }
    // else if (c == '"') -1 // this is the EOS for the caller
    else if (c < ' ')
      throw new Exception("invalid control in string")
    else c.toInt
  }

  // callers expect to get an EOB so this is rare
  def readChar(): Char = {
    read().toChar
  }

  // consumes 4 hex characters after current
  def nextHex4(): Int = {
    var i: Int     = 0
    var accum: Int = 0
    while (i < 4) {
      var c: Int = nextChar.toInt
      // if (c == -1)
      //   throw new Exception("unexpected EOB in string")
      c =
        if ('0' <= c && c <= '9') c - '0'
        else if ('A' <= c && c <= 'F') c - 'A' + 10
        else if ('a' <= c && c <= 'f') c - 'a' + 10
        else
          throw new Exception("invalid charcode in string")
      accum = accum * 16 + c
      i += 1
    }
    accum
  }
}
