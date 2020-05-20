package org.morphir.sdk

import org.morphir.sdk.Maybe.Maybe

private[sdk] trait MaybeLike[+A] extends IterableOnce[A] with Product with Serializable

private[sdk] object MaybeLike {

  def maybe2Iterable[A](xo: Maybe[A]): Iterable[A] =
    if (xo.isEmpty) Iterable.empty else Iterable.single(xo.get)
}
