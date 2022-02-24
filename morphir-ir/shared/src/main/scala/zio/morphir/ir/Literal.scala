package zio.morphir.ir
import scala.language.implicitConversions
import zio.morphir.ir.ValueModule.Value
import zio.ZEnvironment

sealed trait Literal[+A] { self =>
  def value: A
  def toIRValue: Value.Literal[A, Any] = Value.Literal(self, ZEnvironment.empty)
}
object Literal {
  def boolean(value: Boolean): Bool                         = Bool(value)
  def int(value: Int): WholeNumber                          = WholeNumber(java.math.BigInteger.valueOf(value.toLong))
  def long(value: Long): WholeNumber                        = WholeNumber(java.math.BigInteger.valueOf(value))
  def string(value: java.lang.String): String               = Literal.String(value)
  def wholeNumber(value: java.math.BigInteger): WholeNumber = WholeNumber(value)

  implicit def literalToIRValue[A](literal: Literal[A]): Value.Literal[A, Any] = literal.toIRValue

  final case class Bool(value: scala.Boolean)               extends Literal[scala.Boolean]
  final case class Char(value: scala.Char)                  extends Literal[scala.Char]
  final case class String(value: java.lang.String)          extends Literal[java.lang.String]
  final case class WholeNumber(value: java.math.BigInteger) extends Literal[java.math.BigInteger]
  final case class Float(value: java.math.BigDecimal)       extends Literal[java.math.BigDecimal]

}
