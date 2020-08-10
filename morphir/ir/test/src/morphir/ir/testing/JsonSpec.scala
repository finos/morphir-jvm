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


package morphir.ir.testing

import morphir.ir.json.JsonFacade
import upickle.default._
import zio.console.Console
import zio.{ console, ZIO }
import zio.test._
import zio.test.Assertion._

trait JsonSpec extends JsonFacade {
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
