package morphir.sdk
import morphir.sdk.Maybe.Maybe

import java.math.{ BigDecimal => BigDec, RoundingMode }
import scala.util.control.NonFatal

object Decimal {

  type Decimal = BigDec

  object Decimal {
    def apply(value: BigDec): Decimal                  = value
    def apply(value: scala.BigDecimal): Decimal        = value.bigDecimal
    def apply(value: morphir.sdk.Float.Float): Decimal = BigDecimal.exact(value).bigDecimal
    def apply(value: morphir.sdk.Int.Int): Decimal     = BigDecimal.exact(value).bigDecimal

  }

  /**
   * Absolute value (sets the sign as positive)
   */
  def abs(value: Decimal): Decimal = value.abs()

  def add(a: Decimal)(b: Decimal): Decimal = a.add(b)

  def eq(a: Decimal)(b: Decimal): morphir.sdk.Bool.Bool = a.compareTo(b) == 0

  def fastdiv(a: Decimal)(b: Decimal): Maybe[Decimal] =
    if (b.compareTo(zero) == 0) Maybe.nothing
    else
      try {
        Maybe.just(a.divide(b))
      } catch {
        case NonFatal(_) => Maybe.nothing
      }

  def fromInt(value: morphir.sdk.Basics.Int): Decimal =
    Decimal(value)

  /**
   * Converts a Float to a Decimal
   */
  def fromFloat(value: morphir.sdk.Float.Float): Maybe[Decimal] =
    try {
      Maybe.just(Decimal(value))
    } catch {
      case NonFatal(_) => Maybe.nothing
    }

  def fromString(value: morphir.sdk.String.String): Maybe[Decimal] =
    try {
      Maybe.just(new BigDec(value))
    } catch {
      case NonFatal(_) => Maybe.nothing
    }

  def gt(a: Decimal)(b: Decimal): morphir.sdk.Bool.Bool  = a.compareTo(b) > 0
  def gte(a: Decimal)(b: Decimal): morphir.sdk.Bool.Bool = a.compareTo(b) >= 1

  def lt(a: Decimal)(b: Decimal): morphir.sdk.Bool.Bool = a.compareTo(b) < 0

  def mul(a: Decimal)(b: Decimal): Decimal = a.multiply(b)

  def ne(a: Decimal)(b: Decimal): morphir.sdk.Bool.Bool = a.compareTo(b) != 0

  def round(n: morphir.sdk.Int.Int)(decimal: Decimal): Decimal = {
    // Since morphir's Int is actually a Long this isn't really safe
    val scale = math.min(scala.Int.MaxValue, n).toInt
    decimal.setScale(scale, RoundingMode.CEILING)
  }

  def sub(a: Decimal)(b: Decimal): Decimal = a.subtract(b)

  def toFloat(value: Decimal): morphir.sdk.Float.Float =
    morphir.sdk.Float.Float(value.doubleValue())

  //TODO: Make sure the Elm call and this call return the same value
  def toString(value: Decimal): morphir.sdk.String.String = value.toString

  def truncate(n: morphir.sdk.Int.Int)(decimal: Decimal): Decimal = {
    // Since morphir's Int is actually a Long this isn't really safe
    val scale = math.min(scala.Int.MaxValue, n).toInt
    decimal.setScale(scale, RoundingMode.FLOOR)
  }

  /**
   * The number -1.
   */
  val minusOne: Decimal = BigDecimal.exact(-1).bigDecimal

  val one: Decimal = BigDec.ONE

  val zero: Decimal = BigDec.ZERO

}
