package org.morphir.internal.collection

import scala.collection.generic.{IsIterable}
import scala.language.implicitConversions

package object decorators {
  implicit def iteratorDecorator[A](it: Iterator[A]): IteratorDecorator[A] =
    new IteratorDecorator[A](it)

  implicit def IterableDecorator[C](
      coll: C
  )(implicit it: IsIterable[C]): IterableDecorator[C, it.type] =
    new IterableDecorator(coll)(it)
}
