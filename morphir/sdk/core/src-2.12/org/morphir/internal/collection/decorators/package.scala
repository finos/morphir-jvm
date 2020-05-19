package org.morphir.internal.collection

import scala.collection.generic.IsTraversableLike
import scala.language.implicitConversions

package object decorators {
  implicit def iteratorDecorator[A](it: Iterator[A]): IteratorDecorator[A] =
    new IteratorDecorator[A](it)

  implicit def IterableDecorator[Repr](
      coll: Repr
  )(implicit traversable: IsTraversableLike[Repr]) =
    new IterableDecorator(traversable.conversion(coll))
}
