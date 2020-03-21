package morphir.sdk.json

import zio.test._
import zio.test.Assertion._
import zio.test.environment._
import morphir.sdk.json.Decode.{Error}
import morphir.sdk.core.Result
import morphir.sdk.core.Result.Err
import upickle.default._
import morphir.sdk.core.Maybe

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
    suite("""Decoding using a "oneOf" Decoder:""")(
      suite("Should work as expected")(
        testDecodeString(Decode.oneOf(Decode.int, Decode.`null`(0)))("null")(
          equalTo(DecodeResult.ok(0))
        ),
        testDecodeString(Decode.oneOf(Decode.int, Decode.`null`(0)))("42")(
          equalTo(DecodeResult.ok(42))
        ),
        testDecodeString(Decode.oneOf(Decode.int, Decode.`null`(0)))("false")(
          equalTo(
            Result.Err[Error, Int](
              Error.oneOf(
                Decode.Error.Failure("Expecting an INT", ujson.Bool(false)),
                Decode.Error.Failure("Expecting null", ujson.Bool(false))
              )
            )
          )
        )
      )
    ),
    suite("""Decoding using a "field" Decoder:""")(
      suite("Should work as expected")(
        testDecodeString(Decode.field("name")(Decode.string))(
          """{"name":"John Smith"}"""
        )(
          equalTo(DecodeResult.ok("John Smith"))
        ),
        testDecodeJsonValue(Decode.field("age")(Decode.string))(
          ujson.read("""{"name":"John Smith"}""")
        )(jsonValue =>
          equalTo(
            DecodeResult.errorExpecting(
              "an OBJECT with a field named 'age'",
              jsonValue
            )
          )
        )
      )
    ),
    suite("""Decoding using a "map" Decoder:""")(
      suite("Should work as expected")(
        testDecodeString(
          Decode.map((i: Int) => i.toString)(Decode.int)
        )("42", Some("mapping decoder"))(
          equalTo(DecodeResult.ok("42"))
        ),
        testDecodeString(
          Decode.map[String, Boolean] {
            case "Y" | "y" | "true" | "True" => true
            case _                           => false
          }(Decode.string)
        )("\"Y\"", Some("mapping decoder"))(
          equalTo(DecodeResult.ok(true))
        )
      )
    ),
    suite("""Decoding using a "map2" Decoder:""")(
      suite("Should work as expected")(
        testDecodeJsonValue(
          Decode.map2(Point)(Decode.field("x")(Decode.float))(
            Decode.field("y")(Decode.float)
          )
        )(ujson.read("""{"x": 3, "y":4}"""))(_ =>
          equalTo(DecodeResult.ok(Point(3, 4)))
        )
      )
    ),
    suite("""Decoding using a "map3" Decoder:""")(
      suite("Should work as expected")(
        testDecodeJsonValue(
          Decode.map3(Person)(Decode.decodeField("firstName", Decode.string))(
            Decode.decodeField("lastName", Decode.string)
          )(Decode.decodeField("age", Decode.float))
        )(
          ujson.read("""{"firstName": "John", "lastName": "Doe", "age":30.5}""")
        )(_ => equalTo(DecodeResult.ok(Person("John", "Doe", 30.5f)))),
        testDecodeJsonValue(
          Decode.map3(Person)(Decode.decodeField("firstName", Decode.string))(
            Decode.decodeField("lastName", Decode.string)
          )(Decode.decodeField("age", Decode.float))
        )(
          ujson.read("""{"firstName": "John", "lastName": "Doe", "sex":"M"}""")
        )(jsonValue =>
          equalTo(
            DecodeResult.errorExpecting(
              "an OBJECT with a field named 'age'",
              jsonValue
            )
          )
        )
      )
    ),
    suite("""Decoding using a "list" Decoder:""")(
      suite("Should work as expected")(
        testDecodeJsonValue(Decode.list(Decode.int))(ujson.read("[1,2,3,4]"))(
          _ => equalTo(DecodeResult.ok(List(1, 2, 3, 4)))
        ),
        testDecodeJsonValue(Decode.list(Decode.bool))(
          ujson.read("[true,false]")
        )(_ => equalTo(DecodeResult.ok(List(true, false)))),
        testDecodeJsonValue(Decode.list(Decode.int))(
          ujson.read("{}")
        )(jsonValue =>
          equalTo(DecodeResult.errorExpecting("a LIST", jsonValue))
        )
      )
    ),
    suite("""Decoding using a "array" Decoder:""")(
      suite("Should work as expected")(
        testDecodeJsonValue(Decode.array(Decode.int))(ujson.read("[1,2,3,4]"))(
          _ =>
            isCase(
              "Ok",
              (result: DecodeResult[Array[Int]]) =>
                result match {
                  case Result.Ok(items) => Some(items)
                  case Result.Err(_)    => None
                },
              equalTo(Array(1, 2, 3, 4))
            )
        ),
        testDecodeJsonValue(Decode.array(Decode.bool))(
          ujson.read("[true,false]")
        )(_ =>
          isCase(
            "Ok",
            (result: DecodeResult[Array[Boolean]]) =>
              result match {
                case Result.Ok(items) => Some(items)
                case Result.Err(_)    => None
              },
            equalTo(Array(true, false))
          )
        ),
        testDecodeJsonValue(Decode.array(Decode.int))(
          ujson.read("{}")
        )(jsonValue =>
          equalTo(DecodeResult.errorExpecting("an ARRAY", jsonValue))
        )
      )
    ),
    suite("""Decoding using a "nullable" Decoder:""")(
      suite("Should work as expected")(
        testDecodeString(Decode.nullable(Decode.int))("13")(
          equalTo(DecodeResult.ok(Maybe.Just(13)))
        ),
        testDecodeString(Decode.nullable(Decode.int))("42")(
          equalTo(DecodeResult.ok(Maybe.Just(42)))
        ),
        testDecodeString(Decode.nullable(Decode.int))("null")(
          equalTo(DecodeResult.ok(Maybe.Nothing))
        ),
        testDecodeString(Decode.nullable(Decode.int))("true")(
          equalTo(
            DecodeResult.err(
              Error.oneOf(
                Error.Failure("Expecting null", ujson.read("true")),
                Error.Failure("Expecting an INT", ujson.read("true"))
              )
            )
          )
        )
      )
    ),
    suite("""Decoding using a "keyValuePairs" Decoder:""")(
      suite("Should work as expected")(
        testDecodeString(Decode.keyValuePairs(Decode.int))(
          """{"alice":42, "bob":99}"""
        )(
          equalTo(DecodeResult.ok(List("alice" -> 42, "bob" -> 99)))
        ),
        testDecodeString(Decode.keyValuePairs(Decode.int))(
          "null"
        )(
          equalTo(DecodeResult.errorExpecting("an OBJECT", ujson.Null))
        ),
        testDecodeString(Decode.keyValuePairs(Decode.int))(
          "[]"
        )(
          equalTo(DecodeResult.errorExpecting("an OBJECT", ujson.Arr()))
        )
      )
    ),
    suite("""Decoding using a "maybe" Decoder:""")(
      suite("Should work as expected")(
        //json = """{ "name": "tom", "age": 42 }"""

        // decodeString (maybe (field "age"    int  )) json == Ok (Just 42)
        testDecodeJsonString(
          Decode.maybe(Decode.field("age")(Decode.int)),
          """{"name":"tom", "age":42}""",
          Some("maybe field age as int")
        )(_ => equalTo(DecodeResult.ok(Maybe.just(42)))),
        //decodeString (maybe (field "name"   int  )) json == Ok Nothing
        testDecodeJsonString(
          Decode.maybe(Decode.field("name")(Decode.int)),
          """{"name":"tom", "age":42}""",
          Some("maybe field name as int")
        )(_ => equalTo(DecodeResult.ok(Maybe.Nothing))),
        //    decodeString (maybe (field "height" float)) json == Ok Nothing
        testDecodeJsonString(
          Decode.maybe(Decode.field("height")(Decode.float)),
          """{"name":"tom", "age":42}""",
          Some("maybe field height as float")
        )(_ => equalTo(DecodeResult.ok(Maybe.Nothing))),
        // decodeString (field "age"    (maybe int  )) json == Ok (Just 42)
        testDecodeJsonString(
          Decode.field("age")(Decode.maybe(Decode.int)),
          """{"name":"tom", "age":42}""",
          Some("field age maybe int")
        )(_ => equalTo(DecodeResult.ok(Maybe.just(42)))),
        //decodeString (field "name"   (maybe int  )) json == Ok Nothing
        testDecodeJsonString(
          Decode.field("name")(Decode.maybe(Decode.int)),
          """{"name":"tom", "age":42}""",
          Some("field name maybe int")
        )(_ => equalTo(DecodeResult.ok(Maybe.Nothing))),
        //decodeString (field "height" (maybe float)) json == Err ...
        testDecodeJsonString(
          Decode.field("height")(Decode.maybe(Decode.int)),
          """{"name":"tom", "age":42}""",
          Some("field height maybe float")
        )(jsonValue =>
          equalTo(
            DecodeResult.errorExpecting(
              "an OBJECT with a field named 'height'",
              jsonValue
            )
          )
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

  def testDecodeString[A](
      decoder: Decoder[A]
  )(json: String, decoderName: Option[String] = None)(
      assertion: Assertion[DecodeResult[A]]
  ) = {
    test(
      s"""Decoding json: $json with decoder: "${decoderName getOrElse (decoder
        .toString())}" should satisfy assertion that the result is: $assertion"""
    ) {
      val result = Decode.decodeString(decoder)(json)
      assert(result)(assertion)
    }
  }

  def testDecodeJsonString[A](
      decoder: Decoder[A],
      json: String,
      decoderName: Option[String] = None
  )(
      getAssertion: ujson.Value => Assertion[DecodeResult[A]]
  ) = {
    val jsonValue = ujson.read(json)
    val assertion = getAssertion(jsonValue)
    test(
      s"""Decoding json: $json with decoder: "${decoderName getOrElse (decoder
        .toString())}" should satisfy assertion that the result is: $assertion"""
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

  def testDecodeJsonValue[A](
      decoder: Decoder[A]
  )(jsonValue: ujson.Value, decoderName: Option[String] = None)(
      getAssertion: ujson.Value => Assertion[DecodeResult[A]]
  ) = {
    val assertion = getAssertion(jsonValue)
    test(
      s"""Decoding json: $jsonValue with decoder: "${decoderName getOrElse (decoder
        .toString())}" should satisfy assertion that the result is: $assertion"""
    ) {
      val result = Decode.decodeValue(decoder)(jsonValue)
      assert(result)(assertion)
    }
  }

  case class Widget(name: String)
  case class Point(x: Float, y: Float)
  case class Person(firstName: String, lastName: String, age: Float)
}
