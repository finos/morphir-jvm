package morphir.ir

import cats.syntax.functor._
import io.circe.{ Decoder, Encoder }
import io.circe.syntax._

sealed abstract class Literal(val tag: String) extends Product with Serializable {
  type A
  def value: A
}

sealed abstract class LiteralT[T](tag: String) extends Literal(tag) {
  type A = T
  def value: A
}

sealed abstract class LiteralCompanion(val Tag: String) {
  def hasMatchingTag[P <: Product](product: P): Boolean =
    if (product.productArity < 1) false
    else product.productElement(0) == Tag
}

object Literal {
  type Aux[A0] = Literal { type A = A0 }

  def bool(value: Boolean): BoolLiteral    = BoolLiteral(value)
  def char(value: Char): CharLiteral       = CharLiteral(value)
  def string(value: String): StringLiteral = StringLiteral(value)
  def int(value: Int): IntLiteral          = IntLiteral(value)
  def float(value: Float): FloatLiteral    = FloatLiteral(value)

  final case class BoolLiteral(value: Boolean)  extends LiteralT[Boolean](BoolLiteral.Tag)
  final case class CharLiteral(value: Char)     extends LiteralT[Char](CharLiteral.Tag)
  final case class StringLiteral(value: String) extends LiteralT[String](StringLiteral.Tag)
  final case class IntLiteral(value: Int)       extends LiteralT[Int](IntLiteral.Tag)
  final case class FloatLiteral(value: Float)   extends LiteralT[Float](FloatLiteral.Tag)

  object BoolLiteral extends LiteralCompanion("bool_literal") {
    implicit val encodeBoolLiteral: Encoder[BoolLiteral] =
      Encoder.encodeTuple2[String, Boolean].contramap(v => v.tag -> v.value)

    implicit val decodeBoolLiteral: Decoder[BoolLiteral] =
      Decoder
        .decodeTuple2[String, Boolean]
        .ensure(hasMatchingTag, s"""The tag of a boolean literal must be "$Tag".""")
        .map {
          case (_, value) => BoolLiteral(value)
        }
  }

  object CharLiteral extends LiteralCompanion("char_literal") {
    implicit val encodeCharLiteral: Encoder[CharLiteral] =
      Encoder.encodeTuple2[String, Char].contramap(v => v.tag -> v.value)

    implicit val decodeCharLiteral: Decoder[CharLiteral] =
      Decoder
        .decodeTuple2[String, Char]
        .ensure(hasMatchingTag, s"""The tag of a char literal must be "$Tag".""")
        .map { case (_, value) => CharLiteral(value) }
  }

  object StringLiteral extends LiteralCompanion("string_literal") {
    implicit val encodeStringLiteral: Encoder[StringLiteral] =
      Encoder.encodeTuple2[String, String].contramap(v => v.tag -> v.value)

    implicit val decodeStringLiteral: Decoder[StringLiteral] =
      Decoder
        .decodeTuple2[String, String]
        .ensure(hasMatchingTag, s"""The tag of a string literal must be "$Tag".""")
        .map { case (_, value) => StringLiteral(value) }

  }

  object IntLiteral extends LiteralCompanion("int_literal") {
    implicit val encodeIntLiteral: Encoder[IntLiteral] =
      Encoder.encodeTuple2[String, Int].contramap(v => v.tag -> v.value)

    implicit val decodeIntLiteral: Decoder[IntLiteral] =
      Decoder
        .decodeTuple2[String, Int]
        .ensure(hasMatchingTag, s"""The tag of a int literal must be "$Tag".""")
        .map { case (_, value) => IntLiteral(value) }
  }

  object FloatLiteral extends LiteralCompanion("float_literal") {
    implicit val encodeFloatLiteral: Encoder[FloatLiteral] =
      Encoder.encodeTuple2[String, Float].contramap(v => v.tag -> v.value)

    implicit val decodeFloatLiteral: Decoder[FloatLiteral] =
      Decoder
        .decodeTuple2[String, Float]
        .ensure(hasMatchingTag, s"""The tag of a float literal must be "$Tag".""")
        .map { case (_, value) => FloatLiteral(value) }

  }

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
