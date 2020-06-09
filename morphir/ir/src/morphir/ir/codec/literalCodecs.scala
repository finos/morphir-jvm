package morphir.ir.codec

import io.circe.{ Decoder, Encoder }
import morphir.ir.core.TaggedCompanionObjectLike
import morphir.ir.json.Decode.DecodeError
import morphir.ir.literal.Literal
import morphir.ir.literal.Literal._
import upickle.default._

object literalCodecs {
  trait LiteralCodec {
    implicit def readWriter[A: ReadWriter]: ReadWriter[Literal] =
      readwriter[ujson.Value].bimap[Literal](
        {
          case literal: BoolLiteral   => writeJs(literal)
          case literal: CharLiteral   => writeJs(literal)
          case literal: StringLiteral => writeJs(literal)
          case literal: IntLiteral    => writeJs(literal)
          case literal: FloatLiteral  => writeJs(literal)
        },
        json => {
          val head = json(0).str
          head match {
            case tag => throw DecodeError.unexpectedTag(tag, List.empty)
          }
        }
      )
  }

  trait BoolLiteralCodec extends TaggedCompanionObjectLike {
    val Tag: String = "bool_literal"

    implicit val readWriter: ReadWriter[BoolLiteral] =
      readwriter[(String, Boolean)].bimap(
        literal => (literal.tag, literal.value), {
          case (tag, value) if tag == Tag => BoolLiteral(value)
          case (tag, _)                   => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )

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

  trait CharLiteralCodec extends TaggedCompanionObjectLike {
    val Tag: String = "char_literal"

    implicit val readWriter: ReadWriter[CharLiteral] =
      readwriter[(String, Char)].bimap(
        literal => (literal.tag, literal.value), {
          case (tag, value) if tag == Tag => CharLiteral(value)
          case (tag, _)                   => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )

    implicit val encodeCharLiteral: Encoder[CharLiteral] =
      Encoder.encodeTuple2[String, Char].contramap(v => v.tag -> v.value)

    implicit val decodeCharLiteral: Decoder[CharLiteral] =
      Decoder
        .decodeTuple2[String, Char]
        .ensure(hasMatchingTag, s"""The tag of a char literal must be "$Tag".""")
        .map { case (_, value) => CharLiteral(value) }
  }

  trait StringLiteralCodec extends TaggedCompanionObjectLike {
    val Tag: String = "string_literal"

    implicit val readWriter: ReadWriter[StringLiteral] =
      readwriter[(String, String)].bimap(
        literal => (literal.tag, literal.value), {
          case (tag, value) if tag == Tag => StringLiteral(value)
          case (tag, _)                   => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )

    implicit val encodeStringLiteral: Encoder[StringLiteral] =
      Encoder.encodeTuple2[String, String].contramap(v => v.tag -> v.value)

    implicit val decodeStringLiteral: Decoder[StringLiteral] =
      Decoder
        .decodeTuple2[String, String]
        .ensure(hasMatchingTag, s"""The tag of a string literal must be "$Tag".""")
        .map { case (_, value) => StringLiteral(value) }

  }

  trait IntLiteralCodec extends TaggedCompanionObjectLike {
    val Tag: String = "int_literal"

    implicit val readWriter: ReadWriter[IntLiteral] =
      readwriter[(String, Int)].bimap(
        literal => (literal.tag, literal.value), {
          case (tag, value) if tag == Tag => IntLiteral(value)
          case (tag, _)                   => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )

    implicit val encodeIntLiteral: Encoder[IntLiteral] =
      Encoder.encodeTuple2[String, Int].contramap(v => v.tag -> v.value)

    implicit val decodeIntLiteral: Decoder[IntLiteral] =
      Decoder
        .decodeTuple2[String, Int]
        .ensure(hasMatchingTag, s"""The tag of a int literal must be "$Tag".""")
        .map { case (_, value) => IntLiteral(value) }
  }

  trait FloatLiteralCodec extends TaggedCompanionObjectLike {
    val Tag: String = "float_literal"

    implicit val readWriter: ReadWriter[FloatLiteral] =
      readwriter[(String, Float)].bimap(
        literal => (literal.tag, literal.value), {
          case (tag, value) if tag == Tag => FloatLiteral(value)
          case (tag, _)                   => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )

    implicit val encodeFloatLiteral: Encoder[FloatLiteral] =
      Encoder.encodeTuple2[String, Float].contramap(v => v.tag -> v.value)

    implicit val decodeFloatLiteral: Decoder[FloatLiteral] =
      Decoder
        .decodeTuple2[String, Float]
        .ensure(hasMatchingTag, s"""The tag of a float literal must be "$Tag".""")
        .map { case (_, value) => FloatLiteral(value) }
  }
}
