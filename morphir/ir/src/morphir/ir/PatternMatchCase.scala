package morphir.ir

import io.circe.{ Decoder, Encoder }

case class PatternMatchCase[+A](pattern: Pattern[A], value: Value[A]) {

  def mapAttributes[B](f: A => B): PatternMatchCase[B] =
    PatternMatchCase(pattern.mapAttributes(f), value.mapAttributes(f))

  @inline def toTuple: (Pattern[A], Value[A]) = pattern -> value
}

object PatternMatchCase {
  implicit def encodePatternMatchCase[A: Encoder]: Encoder[PatternMatchCase[A]] =
    Encoder.encodeTuple2[Pattern[A], Value[A]].contramap(_.toTuple)

  implicit def decodePatternMatchCase[A: Decoder]: Decoder[PatternMatchCase[A]] =
    Decoder.decodeTuple2[Pattern[A], Value[A]].map {
      case (pattern, value) => PatternMatchCase(pattern, value)
    }
}
