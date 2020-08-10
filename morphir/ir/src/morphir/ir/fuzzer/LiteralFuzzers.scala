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


package morphir.ir.fuzzer

import morphir.ir.literal
import morphir.ir.literal._
import morphir.ir.literal.Literal._

import zio.test.Gen

trait LiteralFuzzers {
  implicit val fuzzBoolLiteral: Fuzzer[BoolLiteral] =
    Gen.boolean.map(literal.bool)

  implicit val fuzzCharLiteral: Fuzzer[CharLiteral] =
    Gen.anyUnicodeChar.map(literal.char)

  implicit val fuzzStringLiteral: Fuzzer[StringLiteral] =
    Gen.alphaNumericString.map(literal.string)

  implicit val fuzzIntLiteral: Fuzzer[IntLiteral] =
    Gen.anyInt.map(literal.int)

  implicit val fuzzFloatLiteral: Fuzzer[FloatLiteral] =
    Gen.anyFloat.map(literal.float)

  implicit val fuzzLiteral: Fuzzer[Literal] =
    Gen.oneOf(fuzzBoolLiteral, fuzzCharLiteral, fuzzStringLiteral, fuzzIntLiteral, fuzzFloatLiteral)
}

object LiteralFuzzers extends LiteralFuzzers
