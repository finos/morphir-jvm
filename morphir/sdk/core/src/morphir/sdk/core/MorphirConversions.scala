package morphir.sdk.core
import scala.{Char => ScalaChar}

trait MorphirConversions {
  implicit def asMorphir(ch: ScalaChar): Char =
    Char.from(ch)
}

object MorphirConversions extends MorphirConversions
