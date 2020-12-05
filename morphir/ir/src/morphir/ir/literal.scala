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

package morphir.ir

import morphir.ir.codec.literalCodecs

object literal {
  def bool(value: Boolean): Literal.BoolLiteral = Literal.BoolLiteral(value)

  def char(value: Char): Literal.CharLiteral = Literal.CharLiteral(value)

  def string(value: String): Literal.StringLiteral = Literal.StringLiteral(value)

  def int(value: Int): Literal.IntLiteral = Literal.IntLiteral(value)

  def float(value: Float): Literal.FloatLiteral = Literal.FloatLiteral(value)

  sealed abstract class Literal(val tag: String) extends Product with Serializable {
    type ValueType

    def value: ValueType
  }

  object Literal extends literalCodecs.LiteralCodec {
    type Aux[A0] = Literal { type A = A0 }

    final case class BoolLiteral(value: Boolean) extends Literal(BoolLiteral.Tag) {
      type ValueType = Boolean
    }
    object BoolLiteral extends literalCodecs.BoolLiteralCodec

    final case class CharLiteral(value: Char) extends Literal(CharLiteral.Tag) {
      type ValueType = Char
    }
    object CharLiteral extends literalCodecs.CharLiteralCodec

    final case class StringLiteral(value: String) extends Literal(StringLiteral.Tag) {
      type ValueType = String
    }
    object StringLiteral extends literalCodecs.StringLiteralCodec

    final case class IntLiteral(value: Int) extends Literal(IntLiteral.Tag) {
      type ValueType = Int
    }
    object IntLiteral extends literalCodecs.IntLiteralCodec

    final case class FloatLiteral(value: Float) extends Literal(FloatLiteral.Tag) {
      type ValueType = Float
    }
    object FloatLiteral extends literalCodecs.FloatLiteralCodec
  }

}
