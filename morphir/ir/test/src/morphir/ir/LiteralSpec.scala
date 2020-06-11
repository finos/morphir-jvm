package morphir.ir

import zio.test._
import zio.test.Assertion._
import zio.test.TestAspect._
import morphir.ir.testing.JsonSpec
import morphir.ir.fuzzer.LiteralFuzzers._

import morphir.ir.literal.Literal._

object LiteralSpec extends DefaultRunnableSpec with JsonSpec {
  def spec =
    suite("Literal Spec")(
      suite("JSON encoding/decoding")(
        testM("Any literal value should encode")(
          check(fuzzLiteral)((sut: literal.Literal) =>
            assert(compactEncode(sut))(
              anything
            )
          )
        )
      ),
      suite("BoolLiteral Spec")(
        suite("JSON encoding/decoding")(
          testM("BoolLiteral should encode as a JSON Array")(
            check(Gen.boolean) { input =>
              val sut = BoolLiteral(input)
              assert(encodeAsJson(sut))(equalTo(ujson.Arr(ujson.Str("bool_literal"), ujson.Bool(input))))
            }
          ),
          testM("BoolLiteral should have a well behaving JSON codec")(
            checkM(Gen.boolean) { input =>
              val original = BoolLiteral(input)
              checkCodecIsWellBehaved(original)
            }
          )
        )
      ),
      suite("CharLiteral Spec")(
        suite("JSON encoding/decoding")(
          testM("CharLiteral should encode as a JSON Array")(
            check(Gen.anyChar) { input =>
              val sut = CharLiteral(input)
              assert(encodeAsJson(sut))(equalTo(ujson.Arr(ujson.Str("char_literal"), encodeAsJson(input))))
            }
          ),
          testM("CharLiteral should have a well behaving JSON codec")(
            checkM(Gen.anyChar) { input =>
              val original = CharLiteral(input)
              checkCodecIsWellBehaved(original)
            }
          )
        )
      ),
      suite("StringLiteral Spec")(
        suite("JSON encoding/decoding")(
          testM("StringLiteral should encode as a JSON Array")(
            check(Gen.anyString) { input =>
              val sut = StringLiteral(input)
              assert(encodeAsJson(sut))(
                equalTo(ujson.Arr(ujson.Str("string_literal"), encodeAsJson(input)))
              )
            }
          ),
          testM("StringLiteral should have a well behaving JSON codec")(
            checkM(Gen.anyString) { input =>
              val original = StringLiteral(input)
              checkCodecIsWellBehaved(original)
            }
          )
        )
      ),
      suite("IntLiteral Spec")(
        suite("JSON encoding/decoding")(
          testM("IntLiteral should encode as a JSON Array")(
            check(Gen.anyInt) { input =>
              val sut = IntLiteral(input)
              assert(encodeAsJson(sut))(equalTo(ujson.Arr(ujson.Str("int_literal"), encodeAsJson(input))))
            }
          ),
          testM("IntLiteral should have a well behaving JSON codec")(
            checkM(Gen.anyInt) { input =>
              val original = IntLiteral(input)
              checkCodecIsWellBehaved(original)
            }
          )
        )
      ),
      suite("FloatLiteral Spec")(
        suite("JSON encoding/decoding")(
          testM("FloatLiteral should encode as a JSON Array")(
            check(Gen.anyFloat) { input =>
              val sut = FloatLiteral(input)
              assert(encodeAsJson(sut))(equalTo(ujson.Arr(ujson.Str("float_literal"), encodeAsJson(input))))
            }
          ),
          testM("FloatLiteral should have a well behaving JSON codec")(
            checkM(Gen.anyFloat) { input =>
              val original = FloatLiteral(input)
              checkCodecIsWellBehaved(original)
            }
          )
        )
      )
    ) @@ silent
}
