package morphir.ir

import cats.syntax.functor._
import io.circe.{ Decoder, Encoder }
import io.circe.syntax._
import morphir.ir.codec.literalCodecs
import morphir.ir.core.TaggedCompanionObject
import morphir.ir.literal.Literal.{ BoolLiteral, CharLiteral, FloatLiteral, IntLiteral, StringLiteral }

object literal {

  def bool(value: Boolean): BoolLiteral = BoolLiteral(value)

  def char(value: Char): CharLiteral = CharLiteral(value)

  def string(value: String): StringLiteral = StringLiteral(value)

  def int(value: Int): IntLiteral = IntLiteral(value)

  def float(value: Float): FloatLiteral = FloatLiteral(value)

  sealed abstract class Literal(val tag: String) extends Product with Serializable {
    type A

    def value: A
  }

  sealed abstract class LiteralT[T](tag: String) extends Literal(tag) {
    type A = T

    def value: A
  }

  sealed abstract class LiteralCompanion(tag: String) extends TaggedCompanionObject(tag)

  object Literal {
    type Aux[A0] = Literal { type A = A0 }

    final case class BoolLiteral(value: Boolean) extends LiteralT[Boolean](BoolLiteral.Tag)
    object BoolLiteral                           extends literalCodecs.BoolLiteralCodec

    final case class CharLiteral(value: Char) extends LiteralT[Char](CharLiteral.Tag)
    object CharLiteral                        extends literalCodecs.CharLiteralCodec

    final case class StringLiteral(value: String) extends LiteralT[String](StringLiteral.Tag)
    object StringLiteral                          extends literalCodecs.StringLiteralCodec

    final case class IntLiteral(value: Int) extends LiteralT[Int](IntLiteral.Tag)
    object IntLiteral                       extends literalCodecs.IntLiteralCodec

    final case class FloatLiteral(value: Float) extends LiteralT[Float](FloatLiteral.Tag)
    object FloatLiteral                         extends literalCodecs.FloatLiteralCodec

    implicit val encodeLiteral: Encoder[Literal] = Encoder.instance {
      case lit @ BoolLiteral(_)   => lit.asJson
      case lit @ CharLiteral(_)   => lit.asJson
      case lit @ StringLiteral(_) => lit.asJson
      case lit @ IntLiteral(_)    => lit.asJson
      case lit @ FloatLiteral(_)  => lit.asJson
    }

    implicit def decodeLiteral: Decoder[Literal] =
      Decoder[BoolLiteral].widen[Literal]

  }

}
