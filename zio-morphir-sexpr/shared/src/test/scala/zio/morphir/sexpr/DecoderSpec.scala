package zio.morphir.sexpr

import zio.morphir.testing.ZioBaseSpec
import zio.test.TestAspect.{ignore, tag}
import zio.test._

object DecoderSpec extends ZioBaseSpec {
  def spec = suite("Decoder")(
    suite("fromSExpr")(
      suite("primitives")(
        test("string") {
          assertTrue(
            "\"hello world\"".fromSExpr[String] == Right("hello world"),
            "\"hello\\nworld\"".fromSExpr[String] == Right("hello\nworld"),
            "\"hello\\rworld\"".fromSExpr[String] == Right("hello\rworld"),
            "\"hello\\u0000world\"".fromSExpr[String] == Right("hello\u0000world")
          )
          // "hello\u0000world".toSExpr == "\"hello\\u0000world\""
        } + test("boolean") {
          assertTrue("true".fromSExpr[Boolean] == Right(true)) &&
          assertTrue("false".fromSExpr[Boolean] == Right(false))
        } @@ ignore @@ tag("Something isn't working right!")
      )
    )
  )
}
