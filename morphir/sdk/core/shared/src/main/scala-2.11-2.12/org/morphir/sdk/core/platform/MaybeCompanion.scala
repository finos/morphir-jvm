package org.morphir.sdk.core.platform

import org.morphir.sdk.core.Maybe.Maybe
import scala.language.implicitConversions

private[core] trait MaybeCompanion {
  /** An implicit conversion that converts an option to an iterable value */
  implicit def maybe2Iterable[A](xo: Maybe[A]): Iterable[A] =
    if (xo.isEmpty) List.empty.toIterable else List(xo.get).toIterable
}