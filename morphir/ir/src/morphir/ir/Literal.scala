package morphir.ir
import io.circe.{ Decoder, Encoder }

sealed abstract class Literal[+A](val tag: String) extends Product with Serializable

object Literal {

  def bool(value: Boolean): BoolLiteral    = BoolLiteral(value)
  def char(value: Char): CharLiteral       = CharLiteral(value)
  def string(value: String): StringLiteral = StringLiteral(value)
  def int(value: Int): IntLiteral          = IntLiteral(value)
  def float(value: Float): FloatLiteral    = FloatLiteral(value)

  final case class BoolLiteral(value: Boolean)  extends Literal[Boolean]("bool_literal")
  final case class CharLiteral(value: Char)     extends Literal[Char]("char_literal")
  final case class StringLiteral(value: String) extends Literal[String]("string_literal")
  final case class IntLiteral(value: Int)       extends Literal[Int]("int_literal")
  final case class FloatLiteral(value: Float)   extends Literal[Float]("float_literal")

  object BoolLiteral {
    implicit val encodeBoolLiteral: Encoder[BoolLiteral] =
      Encoder.encodeTuple2[String, Boolean].contramap(v => v.tag -> v.value)

    implicit val decodeBoolLiteral: Decoder[BoolLiteral] =
      Decoder.decodeTuple2[String, Boolean].map { case (_, value) => BoolLiteral(value) }
  }

  object CharLiteral {
    implicit val encodeCharLiteral: Encoder[CharLiteral] =
      Encoder.encodeTuple2[String, Char].contramap(v => v.tag -> v.value)

    implicit val decodeCharLiteral: Decoder[CharLiteral] =
      Decoder.decodeTuple2[String, Char].map { case (_, value) => CharLiteral(value) }
  }

  object StringLiteral {
    implicit val encodeStringLiteral: Encoder[StringLiteral] =
      Encoder.encodeTuple2[String, String].contramap(v => v.tag -> v.value)

    implicit val decodeStringLiteral: Decoder[StringLiteral] =
      Decoder.decodeTuple2[String, String].map { case (_, value) => StringLiteral(value) }

  }

  object IntLiteral {
    implicit val encodeIntLiteral: Encoder[IntLiteral] =
      Encoder.encodeTuple2[String, Int].contramap(v => v.tag -> v.value)

    implicit val decodeIntLiteral: Decoder[IntLiteral] =
      Decoder.decodeTuple2[String, Int].map { case (_, value) => IntLiteral(value) }
  }

  object FloatLiteral {
    implicit val encodeFloatLiteral: Encoder[FloatLiteral] =
      Encoder.encodeTuple2[String, Float].contramap(v => v.tag -> v.value)

    implicit val decodeFloatLiteral: Decoder[FloatLiteral] =
      Decoder.decodeTuple2[String, Float].map { case (_, value) => FloatLiteral(value) }

  }
}
