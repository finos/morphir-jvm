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

package morphir.sdk

import zio.test._
import zio.test.Assertion._
import morphir.sdk.{LocalDate => SdkDate}

object LocalDateSpec extends DefaultRunnableSpec {
  val date = java.time.LocalDate.now

  def spec = suite("Addition") {
    suite("Add days") {
      test("Identity: Adding 0") {

        assert(SdkDate.addDays(0, date))(equalTo(date.plusDays(0)))
      }
//      test("Identity: Adding up") {
//        assert(SdkDate.addDays(date, 1)(equalTo(date.plusDays(1))))
//      }
//      test("Identity: Adding down") {
//        assert(SdkDate.addDays(date, -1)(equalTo(date.plusDays(-1))))
//      }
    }
//    suite("Add weeks") {
//      test("Identity: Adding 0") {
//        assert(SdkDate.addWeeks(date, 0)(equalTo(date.plusWeeks(0))))
//      }
//      test("Identity: Adding up") {
//        assert(SdkDate.addWeeks(date, 1)(equalTo(date.plusWeeks(1))))
//      }
//      test("Identity: Adding down") {
//        assert(SdkDate.addWeeks(date, -1)(equalTo(date.plusWeeks(-1))))
//      }
//    };
//    suite("Add months") {
//      test("Identity: Adding 0") {
//        assert(SdkDate.addMonths(date, 0)(equalTo(date.plusMonths(0))))
//      }
//      test("Identity: Adding up") {
//        assert(SdkDate.addMonths(date, 1)(equalTo(date.plusMonths(1))))
//      }
//      test("Identity: Adding down") {
//        assert(SdkDate.addMonths(date, -1)(equalTo(date.plusMonths(-1))))
//      }
//    };
//    suite("Add years") {
//      test("Identity: Adding 0") {
//        assert(SdkDate.addYears(date, 0)(equalTo(date.plusYears(0))))
//      }
//      test("Identity: Adding up") {
//        assert(SdkDate.addYears(date, 1)(equalTo(date.plusYears(1))))
//      }
//      test("Identity: Adding down") {
//        assert(SdkDate.addYears(date, -1)(equalTo(date.plusYears(-1))))
//      }
//    }
  }

  case class Wrapped[A](value: A)
}
