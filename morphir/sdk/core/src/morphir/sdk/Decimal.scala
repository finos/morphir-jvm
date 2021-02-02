package morphir.sdk
import morphir.sdk.Maybe.Maybe
import morphir.sdk.Basics.Order

import java.math.{ BigDecimal => BigDec, RoundingMode }
import scala.util.control.NonFatal

object Decimal {

  type Decimal = BigDec

  object Decimal {
    def apply(value: BigDec): Decimal                  = value
    def apply(value: scala.BigDecimal): Decimal        = value.bigDecimal
    def apply(value: morphir.sdk.Float.Float): Decimal = BigDecimal.exact(value).bigDecimal
    def apply(value: morphir.sdk.Int.Int): Decimal     = BigDecimal(value).bigDecimal

  }

  /**
   * Absolute value (sets the sign as positive)
   */
  def abs(value: Decimal): Decimal = value.abs()

  def add(a: Decimal)(b: Decimal): Decimal = a.add(b)

  def bps(n: morphir.sdk.Int.Int): Decimal = Decimal(n * 0.0001)

  def compare(a: Decimal)(b: Decimal): Order =
    a.compareTo(b) match {
      case 0          => Order.EQ
      case n if n > 0 => Order.GT
      case _          => Order.LT
    }

  def div(a: Decimal)(b: Decimal): Maybe[Decimal] =
    if (b.compareTo(zero) == 0) Maybe.nothing
    else
      try {
        Maybe.just(a.divide(b))
      } catch {
        case NonFatal(_) => Maybe.nothing
      }

  def divWithDefault(default: Decimal)(a: Decimal)(b: Decimal): Decimal =
    Maybe.withDefault(default)(div(a)(b))

  def eq(a: Decimal)(b: Decimal): morphir.sdk.Bool.Bool = a.compareTo(b) == 0

  def fromInt(value: morphir.sdk.Basics.Int): Decimal =
    Decimal(value)

  /**
   * Converts a Float to a Decimal
   */
  def fromFloat(value: morphir.sdk.Float.Float): Decimal =
    Decimal(value)

  def fromString(value: morphir.sdk.String.String): Maybe[Decimal] =
    try {
      Maybe.just(new BigDec(value))
    } catch {
      case NonFatal(_) => Maybe.nothing
    }

  /**
   * Converts an `Int` to a `Decimal`` that represents n hundreds.
   */
  def hundred(n: morphir.sdk.Int.Int): Decimal =
    Decimal(n * 100)

  def hundredth(n: morphir.sdk.Int.Int): Decimal =
    Decimal(n * 0.01)

  def gt(a: Decimal)(b: Decimal): morphir.sdk.Bool.Bool  = a.compareTo(b) > 0
  def gte(a: Decimal)(b: Decimal): morphir.sdk.Bool.Bool = a.compareTo(b) >= 1

  def lt(a: Decimal)(b: Decimal): morphir.sdk.Bool.Bool = a.compareTo(b) < 0

  def million(n: morphir.sdk.Int.Int): Decimal =
    Decimal(n * 1000000)

  def millionth(n: morphir.sdk.Int.Int): Decimal =
    Decimal(n * 0.000001)

  def mul(a: Decimal)(b: Decimal): Decimal = a.multiply(b)

  @inline def ne(a: Decimal)(b: Decimal): morphir.sdk.Bool.Bool = neq(a)(b)
  def neq(a: Decimal)(b: Decimal): morphir.sdk.Bool.Bool        = a.compareTo(b) != 0

  def negate(value: Decimal): Decimal = value.negate()

  def round(decimal: Decimal): Decimal = {
    val scale = decimal.scale()
    decimal.setScale(scale, RoundingMode.HALF_EVEN)
  }

  def shiftDecimalLeft(n: morphir.sdk.Int.Int)(value: Decimal): Decimal =
    value.scaleByPowerOfTen(-n.intValue()) //TODO: When we align Int to Int this should settle in correctly

  def shiftDecimalRight(n: morphir.sdk.Int.Int)(value: Decimal): Decimal =
    value.scaleByPowerOfTen(n.intValue()) //TODO: When we align Int to Int this should settle in correctly

  def sub(a: Decimal)(b: Decimal): Decimal = a.subtract(b)

  def thousand(n: morphir.sdk.Int.Int): Decimal =
    Decimal(n * 1000)

  def toFloat(value: Decimal): morphir.sdk.Float.Float =
    morphir.sdk.Float.Float(value.doubleValue())

  //TODO: Make sure the Elm call and this call return the same value
  def toString(value: Decimal): morphir.sdk.String.String = value.toString

  def truncate(decimal: Decimal): Decimal = {
    // Since morphir's Int is actually a Long this isn't really safe
    val scale = decimal.scale()
    decimal.setScale(scale, RoundingMode.DOWN)
  }

  /**
   * The number -1.
   */
  val minusOne: Decimal = BigDecimal.exact(-1).bigDecimal

  val one: Decimal = BigDec.ONE

  val zero: Decimal = BigDec.ZERO

}
