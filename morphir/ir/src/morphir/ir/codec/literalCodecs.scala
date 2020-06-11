package morphir.ir.codec

import morphir.ir.core.TaggedCompanionObjectLike
import morphir.ir.json.Decode.DecodeError
import morphir.ir.literal.Literal._
import upickle.default._

object literalCodecs {
  trait LiteralCodec {
    implicit val readWriter: ReadWriter[morphir.ir.literal.Literal] =
      readwriter[ujson.Value].bimap(
        {
          case lit: BoolLiteral   => writeJs(lit)
          case lit: CharLiteral   => writeJs(lit)
          case lit: StringLiteral => writeJs(lit)
          case lit: IntLiteral    => writeJs(lit)
          case lit: FloatLiteral  => writeJs(lit)
        },
        json => {
          val head = json(0).str
          head match {
            case tag if tag == BoolLiteral.Tag   => read[BoolLiteral](json)
            case tag if tag == CharLiteral.Tag   => read[CharLiteral](json)
            case tag if tag == StringLiteral.Tag => read[StringLiteral](json)
            case tag if tag == IntLiteral.Tag    => read[IntLiteral](json)
            case tag if tag == FloatLiteral.Tag  => read[FloatLiteral](json)
            case tag =>
              throw DecodeError.unexpectedTag(
                tag,
                BoolLiteral.Tag,
                CharLiteral.Tag,
                StringLiteral.Tag,
                IntLiteral.Tag,
                FloatLiteral.Tag
              )
          }
        }
      )
  }

  object LiteralCodec extends LiteralCodec

  trait BoolLiteralCodec extends TaggedCompanionObjectLike {
    val Tag: String = "bool_literal"

    implicit def readWriter: ReadWriter[BoolLiteral] =
      readwriter[(String, Boolean)].bimap[BoolLiteral](
        literal => (literal.tag, literal.value), {
          case (tag, value) if tag == Tag => BoolLiteral(value)
          case (tag, _)                   => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )
  }

  trait CharLiteralCodec extends TaggedCompanionObjectLike {
    val Tag: String = "char_literal"

    implicit def readWriter: ReadWriter[CharLiteral] =
      readwriter[(String, Char)].bimap[CharLiteral](
        literal => (literal.tag, literal.value), {
          case (tag, value) if tag == Tag => CharLiteral(value)
          case (tag, _)                   => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )
  }

  trait StringLiteralCodec extends TaggedCompanionObjectLike {
    val Tag: String = "string_literal"

    implicit def readWriter: ReadWriter[StringLiteral] =
      readwriter[(String, String)].bimap[StringLiteral](
        literal => (literal.tag, literal.value), {
          case (tag, value) if tag == Tag => StringLiteral(value)
          case (tag, _)                   => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )
  }

  trait IntLiteralCodec extends TaggedCompanionObjectLike {
    val Tag: String = "int_literal"

    implicit def readWriter: ReadWriter[IntLiteral] =
      readwriter[(String, Int)].bimap[IntLiteral](
        literal => (literal.tag, literal.value), {
          case (tag, value) if tag == Tag => IntLiteral(value)
          case (tag, _)                   => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )
  }

  trait FloatLiteralCodec extends TaggedCompanionObjectLike {
    val Tag: String = "float_literal"

    implicit val readWriter: ReadWriter[FloatLiteral] =
      readwriter[(String, Float)].bimap[FloatLiteral](
        literal => (literal.tag, literal.value), {
          case (tag, value) if tag == Tag => FloatLiteral(value)
          case (tag, _)                   => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )
  }
}
