package org.morphir.sdk

import scala.{ Char => ScalaChar }

trait MorphirConversions {
  implicit def asMorphir(ch: ScalaChar): char.Char =
    char.from(ch)
}

object MorphirConversions extends MorphirConversions
