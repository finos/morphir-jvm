package org.morphir.sdk

import org.morphir.sdk.Maybe.Maybe

private[sdk] trait MaybeLike[+A] extends Product with Serializable

private[sdk] object MaybeLike {
  def maybe2Iterable[A](xo: Maybe[A]): Iterable[A] =
    if (xo.isEmpty) List.empty.toIterable else List(xo.get).toIterable
}