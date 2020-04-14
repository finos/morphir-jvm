package org.morphir.sdk

package object core {
  type Maybe[+A] = Option[A]
  type Just[+A] = Maybe.Just[A]
  val Just: Maybe.Just.type = Maybe.Just
}
