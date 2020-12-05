/*
Copyright 2020 Morgan Stanley

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 */

package morphir.ir.codec

import morphir.ir.core.TaggedCompanionObjectLike
import morphir.ir.json.Decode.DecodeError
import morphir.ir.literal.Literal._
import upickle.default._

// scalafix:off DisableSyntax.throw
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
        literal => (literal.tag, literal.value),
        {
          case (tag, value) if tag == Tag => BoolLiteral(value)
          case (tag, _)                   => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )
  }

  trait CharLiteralCodec extends TaggedCompanionObjectLike {
    val Tag: String = "char_literal"

    implicit def readWriter: ReadWriter[CharLiteral] =
      readwriter[(String, Char)].bimap[CharLiteral](
        literal => (literal.tag, literal.value),
        {
          case (tag, value) if tag == Tag => CharLiteral(value)
          case (tag, _)                   => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )
  }

  trait StringLiteralCodec extends TaggedCompanionObjectLike {
    val Tag: String = "string_literal"

    implicit def readWriter: ReadWriter[StringLiteral] =
      readwriter[(String, String)].bimap[StringLiteral](
        literal => (literal.tag, literal.value),
        {
          case (tag, value) if tag == Tag => StringLiteral(value)
          case (tag, _)                   => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )
  }

  trait IntLiteralCodec extends TaggedCompanionObjectLike {
    val Tag: String = "int_literal"

    implicit def readWriter: ReadWriter[IntLiteral] =
      readwriter[(String, Int)].bimap[IntLiteral](
        literal => (literal.tag, literal.value),
        {
          case (tag, value) if tag == Tag => IntLiteral(value)
          case (tag, _)                   => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )
  }

  trait FloatLiteralCodec extends TaggedCompanionObjectLike {
    val Tag: String = "float_literal"

    implicit val readWriter: ReadWriter[FloatLiteral] =
      readwriter[(String, Float)].bimap[FloatLiteral](
        literal => (literal.tag, literal.value),
        {
          case (tag, value) if tag == Tag => FloatLiteral(value)
          case (tag, _)                   => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )
  }
}
// scalafix:on
