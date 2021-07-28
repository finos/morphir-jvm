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

package morphir.internal.collection.decorators

import zio.test.Assertion._
import zio.test._

object IterableDecoratorSpec extends DefaultRunnableSpec {
  def spec = suite("IterableDecoratorSpec")(
    suite("foldSomeLeft Specs")(
      test("Test Case 1:") {
        assert(List.range(0, 100).foldSomeLeft(0)((_, _) => None))(equalTo(0))
      },
      test("Test Case 2:") {
        assert(
          List
            .range(0, 100)
            .foldSomeLeft(0)((_, y) => if (y > 10) None else Some(y))
        )(equalTo(10))
      },
      test("Test Case 3:") {
        assert(
          List
            .range(0, 100)
            .foldSomeLeft(0)((x, y) => if (y > 10) None else Some(x + y))
        )(equalTo(55))
      },
      test("Test Case 4:") {
        assert(
          List
            .range(0, 100)
            .foldSomeLeft(0)((x, y) => Some(x + y))
        )(equalTo(4950))
      },
      test("Test Case 4:") {
        assert(
          List[Int]().foldSomeLeft(10)((x, y) => Some(x + y))
        )(equalTo(10))
      }
    )
  )
}
