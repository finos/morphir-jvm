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

import scala.collection.generic.IsIterable

class IterableDecorator[C, I <: IsIterable[C]](coll: C)(implicit val it: I) {

  /**
   * Left to right fold that stops if the combination function `op`
   * returns `None`
   * @param z the start value
   * @param op the binary operator
   * @tparam B the result type of the binary operator
   * @return the result of inserting `op` between consecutive elements of the collection,
   *         going left to right with the start value `z` on the left, and stopping when
   *         all the elements have been traversed or earlier if the operator returns `None`
   */
  def foldSomeLeft[B](z: B)(op: (B, it.A) => Option[B]): B =
    it(coll).iterator.foldSomeLeft(z)(op)
}
