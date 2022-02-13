package zio.morphir.sexpr.internal

import zio.morphir.sexpr.{SExprError, UnsafeSExpr}

import scala.annotation._

object Lexer {
  val NumberMaxBits: Int = 128

  // True if we got a string (implies a retraction), False for }
  def firstField(trace: List[SExprError], in: RetractReader): Boolean =
    (in.nextNonWhitespace(): @switch) match {
      case '"' =>
        in.retract()
        true
      case '}' => false
      case c =>
        throw UnsafeSExpr(
          SExprError.Message(s"expected string or '}' got '$c'") :: trace
        )
    }

  // True if we got a comma, and False for }
  def nextField(trace: List[SExprError], in: OneCharReader): Boolean =
    (in.nextNonWhitespace(): @switch) match {
      case ',' => true
      case '}' => false
      case c =>
        throw UnsafeSExpr(
          SExprError.Message(s"expected ',' or '}' got '$c'") :: trace
        )
    }

  // TODO: Rename to firstVectorElement
  // True if we got anything besides a ], False for ]
  def firstArrayElement(in: RetractReader): Boolean =
    (in.nextNonWhitespace(): @switch) match {
      case ']' => false
      case _ =>
        in.retract()
        true
    }

  // TODO: Rename to nextVectorElement
  def nextArrayElement(trace: List[SExprError], in: OneCharReader): Boolean =
    (in.nextNonWhitespace(): @switch) match {
      case ',' => true
      case ']' => false
      case c =>
        throw UnsafeSExpr(
          SExprError.Message(s"expected ',' or ']' got '$c'") :: trace
        )
    }

  // avoids allocating lots of strings (they are often the bulk of incoming
  // messages) by only checking for what we expect to see (Jon Pretty's idea).
  //
  // returns the index of the matched field, or -1
  def field(
      trace: List[SExprError],
      in: OneCharReader,
      matrix: StringMatrix
  ): Int = {
    val f = enumeration(trace, in, matrix)
    char(trace, in, ':')
    f
  }

  def enumeration(
      trace: List[SExprError],
      in: OneCharReader,
      matrix: StringMatrix
  ): Int = {
    val stream = streamingString(trace, in)

    var i: Int   = 0
    var bs: Long = matrix.initial
    var c: Int   = -1
    while ({ c = stream.read(); c != -1 }) {
      bs = matrix.update(bs, i, c)
      i += 1
    }
    bs = matrix.exact(bs, i)
    matrix.first(bs)
  }

  private[this] val il: Array[Char]   = "il".toCharArray
  private[this] val alse: Array[Char] = "alse".toCharArray
  private[this] val rue: Array[Char]  = "rue".toCharArray

  def skipValue(trace: List[SExprError], in: RetractReader): Unit =
    (in.nextNonWhitespace(): @switch) match {
      case 'n' => readChars(trace, in, il, "nil")
      case 'f' => readChars(trace, in, alse, "false")
      case 't' => readChars(trace, in, rue, "true")
      case '[' =>
        if (firstArrayElement(in)) {
          while ({
            skipValue(trace, in);
            nextArrayElement(trace, in)
          }) ()
        }
      case '"' =>
        skipString(trace, in)
      case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
        skipNumber(in)
      case c => throw UnsafeSExpr(SExprError.Message(s"unexpected '$c'") :: trace)
    }

  def skipNumber(in: RetractReader): Unit = {
    while (isNumber(in.readChar())) {}
    in.retract()
  }

  def skipString(trace: List[SExprError], in: OneCharReader): Unit = {
    val stream = new EscapedString(trace, in)
    var i: Int = 0
    while ({
      i = stream.read();
      i != -1
    }) ()
  }

  // useful for embedded documents, e.g. CSV contained inside SExpr
  def streamingString(trace: List[SExprError], in: OneCharReader): java.io.Reader = {
    char(trace, in, '"')
    new EscapedString(trace, in)
  }

  def symbol(trace: List[SExprError], in: RetractReader): CharSequence = {
    checkStartSymbol(trace, in)
    val sb = new FastStringBuilder(64)

    // Read the first char
    var c = in.readChar()
    sb.append(c.toChar)
    c = in.readChar()

    while (isSymbolChar(c)) {
      sb.append(c)
      c = in.readChar()
    }
    if (c.isWhitespace)
      sb.buffer
    else
      throw UnsafeSExpr(SExprError.Message(s"${sb.buffer} is followed by unexpected character $c in Symbol") :: trace)
  }

  // non-positional for performance
  @inline private[this] def isFirstSymbolChar(c: Char): Boolean =
    (c == '/' || c == '.' || c.isLetter)

  @inline private[this] def isSymbolChar(c: Char): Boolean =
    (c == '/' || c == '.' || c.isLetterOrDigit)

  // really just a way to consume the whitespace
  private def checkStartSymbol(trace: List[SExprError], in: RetractReader): Unit = {
    (in.nextNonWhitespace(): @switch) match {
      case c if isFirstSymbolChar(c) => ()
      case c =>
        throw UnsafeSExpr(SExprError.Message(s"expected a symbol, got $c") :: trace)
    }
    in.retract()
  }

  def string(trace: List[SExprError], in: OneCharReader): CharSequence = {
    char(trace, in, '"')
    val stream = new EscapedString(trace, in)

    val sb = new FastStringBuilder(64)
    while (true) {
      val c = stream.read()
      if (c == -1)
        return sb.buffer // mutable thing escapes, but cannot be changed
      sb.append(c.toChar)
    }
    throw UnsafeSExpr(SExprError.Message("impossible string") :: trace)
  }

  def boolean(trace: List[SExprError], in: OneCharReader): Boolean =
    (in.nextNonWhitespace(): @switch) match {
      case 't' =>
        readChars(trace, in, rue, "true")
        true
      case 'f' =>
        readChars(trace, in, alse, "false")
        false
      case c =>
        throw UnsafeSExpr(
          SExprError.Message(s"expected 'true' or 'false' got $c") :: trace
        )
    }

  def byte(trace: List[SExprError], in: RetractReader): Byte = {
    checkNumber(trace, in)
    try {
      val i = UnsafeNumbers.byte_(in, false)
      in.retract()
      i
    } catch {
      case UnsafeNumbers.UnsafeNumber =>
        throw UnsafeSExpr(SExprError.Message("expected a Byte") :: trace)
    }
  }

  def short(trace: List[SExprError], in: RetractReader): Short = {
    checkNumber(trace, in)
    try {
      val i = UnsafeNumbers.short_(in, false)
      in.retract()
      i
    } catch {
      case UnsafeNumbers.UnsafeNumber =>
        throw UnsafeSExpr(SExprError.Message("expected a Short") :: trace)
    }
  }

  def int(trace: List[SExprError], in: RetractReader): Int = {
    checkNumber(trace, in)
    try {
      val i = UnsafeNumbers.int_(in, false)
      in.retract()
      i
    } catch {
      case UnsafeNumbers.UnsafeNumber =>
        throw UnsafeSExpr(SExprError.Message("expected an Int") :: trace)
    }
  }

  def long(trace: List[SExprError], in: RetractReader): Long = {
    checkNumber(trace, in)
    try {
      val i = UnsafeNumbers.long_(in, false)
      in.retract()
      i
    } catch {
      case UnsafeNumbers.UnsafeNumber =>
        throw UnsafeSExpr(SExprError.Message("expected a Long") :: trace)
    }
  }

  def bigInteger(
      trace: List[SExprError],
      in: RetractReader
  ): java.math.BigInteger = {
    checkNumber(trace, in)
    try {
      val i = UnsafeNumbers.bigInteger_(in, false, NumberMaxBits)
      in.retract()
      i
    } catch {
      case UnsafeNumbers.UnsafeNumber =>
        throw UnsafeSExpr(SExprError.Message(s"expected a $NumberMaxBits bit BigInteger") :: trace)
    }
  }

  def float(trace: List[SExprError], in: RetractReader): Float = {
    checkNumber(trace, in)
    try {
      val i = UnsafeNumbers.float_(in, false, NumberMaxBits)
      in.retract()
      i
    } catch {
      case UnsafeNumbers.UnsafeNumber =>
        throw UnsafeSExpr(SExprError.Message("expected a Float") :: trace)
    }
  }

  def double(trace: List[SExprError], in: RetractReader): Double = {
    checkNumber(trace, in)
    try {
      val i = UnsafeNumbers.double_(in, false, NumberMaxBits)
      in.retract()
      i
    } catch {
      case UnsafeNumbers.UnsafeNumber =>
        throw UnsafeSExpr(SExprError.Message("expected a Double") :: trace)
    }
  }

  def bigDecimal(
      trace: List[SExprError],
      in: RetractReader
  ): java.math.BigDecimal = {
    checkNumber(trace, in)
    try {
      val i = UnsafeNumbers.bigDecimal_(in, false, NumberMaxBits)
      in.retract()
      i
    } catch {
      case UnsafeNumbers.UnsafeNumber =>
        throw UnsafeSExpr(SExprError.Message(s"expected a $NumberMaxBits BigDecimal") :: trace)
    }
  }

  // optional whitespace and then an expected character
  @inline def char(trace: List[SExprError], in: OneCharReader, c: Char): Unit = {
    val got = in.nextNonWhitespace()
    if (got != c)
      throw UnsafeSExpr(SExprError.Message(s"expected '$c' got '$got'") :: trace)
  }

  @inline def charOnly(
      trace: List[SExprError],
      in: OneCharReader,
      c: Char
  ): Unit = {
    val got = in.readChar()
    if (got != c)
      throw UnsafeSExpr(SExprError.Message(s"expected '$c' got '$got'") :: trace)
  }

  // non-positional for performance
  @inline private[this] def isNumber(c: Char): Boolean =
    (c: @switch) match {
      case '+' | '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '.' | 'e' | 'E' =>
        true
      case _ => false
    }

  // really just a way to consume the whitespace
  private def checkNumber(trace: List[SExprError], in: RetractReader): Unit = {
    (in.nextNonWhitespace(): @switch) match {
      case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => ()
      case c =>
        throw UnsafeSExpr(SExprError.Message(s"expected a number, got $c") :: trace)
    }
    in.retract()
  }

  def readChars(
      trace: List[SExprError],
      in: OneCharReader,
      expect: Array[Char],
      errMsg: String
  ): Unit = {
    var i: Int = 0
    while (i < expect.length) {
      if (in.readChar() != expect(i))
        throw UnsafeSExpr(SExprError.Message(s"expected '$errMsg'") :: trace)
      i += 1
    }
  }

}

// A Reader for the contents of a string, taking care of the escaping.
//
// `read` can throw extra exceptions on badly formed input.
private final class EscapedString(trace: List[SExprError], in: OneCharReader)
    extends java.io.Reader
    with OneCharReader {

  def close(): Unit = in.close()

  private[this] var escaped = false

  override def read(): Int = {
    val c = in.readChar()
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
          throw UnsafeSExpr(SExprError.Message(s"invalid '\\${c.toChar}' in string") :: trace)
      }
    } else if (c == '\\') {
      escaped = true
      read()
    } else if (c == '"') -1 // this is the EOS for the caller
    else if (c < ' ')
      throw UnsafeSExpr(SExprError.Message("invalid control in string") :: trace)
    else c.toInt
  }

  // callers expect to get an EOB so this is rare
  def readChar(): Char = {
    val v = read()
    if (v == -1) throw new UnexpectedEnd
    v.toChar
  }

  // consumes 4 hex characters after current
  def nextHex4(): Int = {
    var i: Int     = 0
    var accum: Int = 0
    while (i < 4) {
      var c: Int = in.read()
      if (c == -1)
        throw UnsafeSExpr(SExprError.Message("unexpected EOB in string") :: trace)
      c =
        if ('0' <= c && c <= '9') c - '0'
        else if ('A' <= c && c <= 'F') c - 'A' + 10
        else if ('a' <= c && c <= 'f') c - 'a' + 10
        else
          throw UnsafeSExpr(SExprError.Message("invalid charcode in string") :: trace)
      accum = accum * 16 + c
      i += 1
    }
    accum
  }
}

// A data structure encoding a simple algorithm for Trie pruning: Given a list
// of strings, and a sequence of incoming characters, find the strings that
// match, by manually maintaining a bitset. Empty strings are not allowed.
final class StringMatrix(val xs: Array[String]) {
  require(xs.forall(_.nonEmpty))
  require(xs.nonEmpty)
  require(xs.length < 64)

  val width               = xs.length
  val height: Int         = xs.map(_.length).max
  val lengths: Array[Int] = xs.map(_.length)
  val initial: Long       = (0 until width).foldLeft(0L)((bs, r) => bs | (1L << r))

  private val matrix: Array[Int] = {
    val m           = Array.fill[Int](width * height)(-1)
    var string: Int = 0
    while (string < width) {
      val s         = xs(string)
      val len       = s.length
      var char: Int = 0
      while (char < len) {
        m(width * char + string) = s.codePointAt(char)
        char += 1
      }
      string += 1
    }
    m
  }

  // must be called with increasing `char` (starting with bitset obtained from a
  // call to 'initial', char = 0)
  def update(bitset: Long, char: Int, c: Int): Long =
    if (char >= height) 0L    // too long
    else if (bitset == 0L) 0L // everybody lost
    else {
      var latest: Long = bitset
      val base: Int    = width * char

      if (bitset == initial) { // special case when it is dense since it is simple
        var string: Int = 0
        while (string < width) {
          if (matrix(base + string) != c)
            latest = latest ^ (1L << string)
          string += 1
        }
      } else {
        var remaining: Long = bitset
        while (remaining != 0L) {
          val string: Int = java.lang.Long.numberOfTrailingZeros(remaining)
          val bit: Long   = 1L << string
          if (matrix(base + string) != c)
            latest = latest ^ bit
          remaining = remaining ^ bit
        }
      }

      latest
    }

  // excludes entries that are not the given exact length
  def exact(bitset: Long, length: Int): Long =
    if (length > height) 0L // too long
    else {
      var latest: Long    = bitset
      var remaining: Long = bitset
      while (remaining != 0L) {
        val string: Int = java.lang.Long.numberOfTrailingZeros(remaining)
        val bit: Long   = 1L << string
        if (lengths(string) != length)
          latest = latest ^ bit
        remaining = remaining ^ bit
      }
      latest
    }

  def first(bitset: Long): Int =
    if (bitset == 0L) -1
    else java.lang.Long.numberOfTrailingZeros(bitset) // never returns 64
}
