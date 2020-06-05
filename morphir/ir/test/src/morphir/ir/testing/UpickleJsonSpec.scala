package morphir.ir.testing

import upickle.default._
import zio.console.Console
import zio.{ console, ZIO }
import zio.test._
import zio.test.Assertion._

trait UpickleJsonSpec {
  def checkCodecIsWellBehaved[A](value: A)(implicit readWriter: ReadWriter[A]): ZIO[Console, Throwable, TestResult] =
    for {
      encoded <- ZIO.effect(writeJs(value))
      _       <- console.putStrLn(s"JSON: ${write(encoded, 2)}")
      decoded <- ZIO.effect(read[A](encoded))
    } yield zio.test.assert(decoded)(equalTo(value))

  def assertCodecIsWellBehaved[A](
    value: A
  )(
    assertion: Assertion[A]
  )(implicit readWriter: ReadWriter[A]): ZIO[Console, Throwable, TestResult] =
    for {
      encoded <- ZIO.effect(writeJs(value))
      _       <- console.putStrLn(s"JSON: ${write(encoded, 2)}")
      decoded <- ZIO.effect(read[A](encoded))
    } yield zio.test.assert(decoded)(assertion)

  def assertEncodesToExpectedCompactJsonString[A](
    value: A
  )(expected: String)(implicit writer: Writer[A]): TestResult =
    assert(write(value))(equalTo(expected))

  def checkEncodesTo[A](value: A)(expected: ujson.Value)(implicit writer: Writer[A]): TestResult =
    assert(writeJs(value))(equalTo(expected))

  def assertEncodesAsExpected[A](
    value: A
  )(assertion: Assertion[ujson.Value])(implicit writer: Writer[A]): TestResult =
    assert(writeJs(value))(assertion)
}
