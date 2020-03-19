package morphir.sdk.json

import zio.test._
import zio.test.Assertion._
import zio.test.environment._
import morphir.sdk.json.Decode.{Decoder, DecodeResult, Error}
import morphir.sdk.core.Result
import morphir.sdk.core.Result.Err
import upickle.default._

object DecodeSpec extends DefaultRunnableSpec {
  def spec = suite("json.Decode Spec")(
    suite("""Decoding using a "succeed" Decoder:""")(
      suite("Should work as expected")(
        testDecodeString(Decode.succeed(42))("true")(
          equalTo(DecodeResult.ok(42))
        ),
        testDecodeString(Decode.succeed(42))("[1,2,3]")(
          equalTo(DecodeResult.ok(42))
        ),
        testDecodeString(Decode.succeed(42))("hello")(
          isCase(
            "DecodeResult", {
              case Result.Err(Error.Failure(message, _)) => Some(message)
              case _                                     => None
            },
            startsWithString("This is not valid JSON ! ")
          )
        )
      )
    ),
    suite("""Decoding using a "fail" Decoder:""")(
      suite("Should work as expected")(
        testDecodeString(Decode.fail[Int]("Decode Bombed!!!"))("true")(
          isCase(
            "DecodeResult", {
              case Result.Err(Error.Failure(message, _)) => Some(message)
              case _                                     => None
            },
            startsWithString("Decode Bombed!!!")
          )
        )
      )
    ),
    suite("""Decoding using a "null" Decoder:""")(
      suite("Should work as expected")(
        testDecodeString(Decode.`null`(false))("null")(
          equalTo(DecodeResult.ok(false))
        ),
        testDecodeString(Decode.`null`(42))("null")(
          equalTo(DecodeResult.ok(42))
        ),
        testDecodeString(Decode.`null`(42))("42")(
          isCase(
            "DecodeResult", {
              case Result.Err(Error.Failure(message, _)) => Some(message)
              case _                                     => None
            },
            startsWithString("Expecting null")
          )
        ),
        testDecodeString(Decode.`null`(42))("false")(
          isCase(
            "DecodeResult", {
              case Result.Err(Error.Failure(message, _)) => Some(message)
              case _                                     => None
            },
            startsWithString("Expecting null")
          )
        )
      )
    ),
    suite("""Decoding using a "value" Decoder:""")(
      testDecodeString(ujson.Obj("name" -> "John"))(Decode.value(_))(
        """{"name":"John"}"""
      )(v => equalTo(DecodeResult.ok(v))),
      testDecodeString(ujson.Arr(1, 2, 3))(Decode.value(_))(
        """[1,2,3]"""
      )(v => equalTo(DecodeResult.ok(v)))
    ),
    suite("""Decoding using a "string" Decoder:""")(
      suite("Should work as expected")(
        testDecodeString(Decode.string)("true")(
          equalTo(
            DecodeResult.errorExpecting("a STRING", read[ujson.Value]("true"))
          )
        ),
        testDecodeString(Decode.string)("42")(
          equalTo(
            DecodeResult.errorExpecting("a STRING", read[ujson.Value]("42"))
          )
        ),
        testDecodeString(Decode.string)("3.14")(
          equalTo(
            DecodeResult.errorExpecting("a STRING", read[ujson.Value]("3.14"))
          )
        ),
        testDecodeString(Decode.string)("\"hello\"")(
          equalTo(DecodeResult.ok("hello"))
        )
      ),
      testDecodeString(Decode.string)("""{"hello": 42}""")(
        equalTo(
          DecodeResult
            .errorExpecting("a STRING", read[ujson.Value]("""{"hello": 42}"""))
        )
      )
    ),
    suite("""Decoding using a "bool" Decoder:""")(
      suite("Should work as expected")(
        testDecodeString(Decode.bool)("true")(
          equalTo(DecodeResult.ok(true))
        ),
        testDecodeString(Decode.bool)("42")(
          equalTo(
            DecodeResult.errorExpecting("a BOOL", read[ujson.Value]("42"))
          )
        ),
        testDecodeString(Decode.bool)("3.14")(
          equalTo(
            DecodeResult.errorExpecting("a BOOL", read[ujson.Value]("3.14"))
          )
        ),
        testDecodeString(Decode.bool)("\"hello\"")(
          equalTo(
            DecodeResult
              .errorExpecting("a BOOL", read[ujson.Value]("\"hello\""))
          )
        )
      )
    ),
    suite("""Decoding using a "int" Decoder:""")(
      suite("Should work as expected")(
        testDecodeString(Decode.int)("true")(
          equalTo(
            DecodeResult.errorExpecting("an INT", read[ujson.Value]("true"))
          )
        ),
        testDecodeString(Decode.int)("42")(
          equalTo(
            DecodeResult.ok(42)
          )
        ),
        testDecodeString(Decode.int)("3.14")(
          equalTo(
            DecodeResult.errorExpecting("an INT", read[ujson.Value]("3.14"))
          )
        ),
        testDecodeString(Decode.int)("\"hello\"")(
          equalTo(
            DecodeResult
              .errorExpecting("an INT", read[ujson.Value]("\"hello\""))
          )
        )
      ),
      testDecodeString(Decode.int)("""{"hello": 42}""")(
        equalTo(
          DecodeResult
            .errorExpecting("an INT", read[ujson.Value]("""{"hello": 42}"""))
        )
      )
    ),
    suite("""Decoding using a "float" Decoder:""")(
      suite("Should work as expected")(
        testDecodeString(Decode.float)("true")(
          equalTo(
            DecodeResult.errorExpecting("a FLOAT", read[ujson.Value]("true"))
          )
        ),
        testDecodeString(Decode.float)("42")(
          equalTo(
            DecodeResult.ok(42.0f)
          )
        ),
        testDecodeString(Decode.float)("3.14")(
          equalTo(
            DecodeResult.ok((3.14f))
          )
        ),
        testDecodeString(Decode.float)("\"hello\"")(
          equalTo(
            DecodeResult
              .errorExpecting("a FLOAT", read[ujson.Value]("\"hello\""))
          )
        )
      ),
      testDecodeString(Decode.float)("""{"hello": 42}""")(
        equalTo(
          DecodeResult
            .errorExpecting("a FLOAT", read[ujson.Value]("""{"hello": 42}"""))
        )
      )
    ),
    suite("Calling indent")(
      test("Should work for things with windows style line endings.") {
        val original = "Line1\r\nLine2\r\nLine3"
        assert(Decode.indent(original))(
          equalTo("Line1\r\n    Line2\r\n    Line3")
        )
      },
      test("Should work for things with linux style line endings.") {
        val original = "Line1\r\nLine2\r\nLine3"
        assert(Decode.indent(original))(
          equalTo("Line1\r\n    Line2\r\n    Line3")
        )
      }
    )
  )

  def testDecodeString[A](decoder: Decoder[A])(json: String)(
      assertion: Assertion[DecodeResult[A]]
  ) = {
    test(
      s"Decoding json: $json with decoder:$decoder should satisfy assertion that the result is: $assertion"
    ) {
      val result = Decode.decodeString(decoder)(json)
      assert(result)(assertion)
    }
  }

  def testDecodeString[A, T](
      input: T
  )(provideDecoder: T => Decoder[A])(json: String)(
      assertion: T => Assertion[DecodeResult[A]]
  ) = {
    test(
      s"Decoding json: $json with a decoder derived from $input should satisfy assertion that the result is: ${assertion(input)}"
    ) {
      val decoder = provideDecoder(input)
      val result = Decode.decodeString(decoder)(json)
      assert(result)(assertion(input))
    }
  }
  case class Widget(name: String)
}
