package zio.morphir.ir
import zio.morphir.ir.Type.UType
import zio.morphir.ir.Value.TypedValue
import zio.morphir.ir.value.RawValue
import zio.morphir.ir.{Value => _}

import scala.language.implicitConversions

sealed trait Literal[+A] { self =>
  def value: A
  def toRawValue: RawValue = Value.Value.Literal.Raw(self)
}

object Literal {
  def boolean(value: Boolean): Bool                         = Bool(value)
  def char(value: scala.Char): Char                         = Char(value)
  def double(value: scala.Double): Float                    = Float(java.math.BigDecimal.valueOf(value))
  def float(value: scala.Float): Float                      = Float(java.math.BigDecimal.valueOf(value.toDouble))
  def int(value: Int): WholeNumber                          = WholeNumber(java.math.BigInteger.valueOf(value.toLong))
  def long(value: Long): WholeNumber                        = WholeNumber(java.math.BigInteger.valueOf(value))
  def string(value: java.lang.String): String               = Literal.String(value)
  def wholeNumber(value: java.math.BigInteger): WholeNumber = WholeNumber(value)

  val False: Bool = Bool(false)
  val True: Bool  = boolean(true)

  implicit def literalToRawValue[A](literal: Literal[A]): RawValue = literal.toRawValue

  final case class Bool(value: scala.Boolean)               extends Literal[scala.Boolean]
  final case class Char(value: scala.Char)                  extends Literal[scala.Char]
  final case class String(value: java.lang.String)          extends Literal[java.lang.String]
  final case class WholeNumber(value: java.math.BigInteger) extends Literal[java.math.BigInteger]
  final case class Float(value: java.math.BigDecimal)       extends Literal[java.math.BigDecimal]

  implicit def LiteralInferredTypeOf[A]: InferredTypeOf[Literal[A]] = new InferredTypeOf[Literal[A]] {
    def inferredType(value: Literal[A]): UType = value match {
      case Bool(_)        => sdk.Basics.boolType
      case Char(_)        => sdk.Char.charType
      case String(_)      => sdk.String.stringType
      case WholeNumber(_) => sdk.Basics.intType
      case Float(_)       => sdk.Basics.floatType
    }
  }

  implicit class LiteralOps[A](private val self: Literal[A]) extends AnyVal {
    def inferredType: UType = InferredTypeOf[Literal[A]].inferredType(self)
    def toTypedValue(implicit ev: InferredTypeOf[Literal[A]]): TypedValue = {
      val tpe = ev.inferredType(self)
      Value.Value.Literal(tpe, self)
    }
  }
}
