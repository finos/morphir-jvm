package morphir.internal.collection.decorators

import scala.collection.generic.IsTraversableLike
import scala.collection.GenTraversableLike

class IterableDecorator[A, Repr](coll: GenTraversableLike[A, Repr]) {
  def foldSomeLeft[B](z: B)(op: (B, A) => Option[B]): B =
    coll.toIterator.foldSomeLeft(z)(op)
}
