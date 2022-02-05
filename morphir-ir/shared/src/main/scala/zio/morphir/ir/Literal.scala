package zio.morphir.ir

sealed trait Literal[+A] {
  def value: A
}
object Literal {
  final case class Bool(value: scala.Boolean)               extends Literal[scala.Boolean]
  final case class Char(value: scala.Char)                  extends Literal[scala.Char]
  final case class String(value: java.lang.String)          extends Literal[java.lang.String]
  final case class WholeNumber(value: java.math.BigInteger) extends Literal[java.math.BigInteger]
  final case class Float(value: java.math.BigDecimal)       extends Literal[java.math.BigDecimal]
}
