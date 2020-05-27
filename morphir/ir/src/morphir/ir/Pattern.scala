package morphir.ir

import cats.syntax.functor._
import io.circe.{ Decoder, Encoder }
import io.circe.syntax._
import morphir.ir.core.TaggedCompanionObject
import morphir.ir.syntax.all._

sealed abstract class Pattern[+A] extends Product with Serializable {
  def attributes: A
  def mapAttributes[B](f: A => B): Pattern[B]
}

object Pattern extends TaggedCompanionObject("pattern") {

  final case class WildcardPattern[+A](attributes: A) extends Pattern[A] {
    def mapAttributes[B](f: A => B): Pattern[B] = WildcardPattern(f(attributes))
  }

  object WildcardPattern extends TaggedCompanionObject("wildcard_pattern") {
    implicit def encodeWildcardPattern[A: Encoder]: Encoder[WildcardPattern[A]] =
      Encoder.encodeTuple2[String, A].contramap(pat => (Tag, pat.attributes))

    implicit def decodeWildcardPattern[A: Decoder]: Decoder[WildcardPattern[A]] =
      Decoder.decodeTuple2[String, A].ensure(hasMatchingTag, s"""The tag of a wildcard pattern must be "$Tag".""").map {
        case (_, attributes) => WildcardPattern(attributes)
      }
  }

  final case class AsPattern[+A](attributes: A, pattern: Pattern[A], name: Name) extends Pattern[A] {
    def mapAttributes[B](f: A => B): Pattern[B] = AsPattern(f(attributes), pattern mapAttributes f, name)
  }

  object AsPattern extends TaggedCompanionObject("as_pattern") {
    implicit def encodeAsPattern[A: Encoder]: Encoder[AsPattern[A]] =
      Encoder.encodeTuple4[String, A, Pattern[A], Name].contramap(pat => (Tag, pat.attributes, pat.pattern, pat.name))

    implicit def decodeAsPattern[A: Decoder]: Decoder[AsPattern[A]] =
      Decoder
        .decodeTuple4[String, A, Pattern[A], Name]
        .ensure(hasMatchingTag, s"""The tag of an as pattern must be "$Tag".""")
        .map {
          case (_, attributes, pattern, name) => AsPattern(attributes, pattern, name)
        }
  }

  final case class TuplePattern[+A](attributes: A, elementPatterns: PatternList[A]) extends Pattern[A] {
    def mapAttributes[B](f: A => B): Pattern[B] = TuplePattern(f(attributes), elementPatterns.mapAttributes(f))
  }

  object TuplePattern extends TaggedCompanionObject("tuple_pattern") {
    implicit def encodeTuplePattern[A: Encoder]: Encoder[TuplePattern[A]] =
      Encoder.encodeTuple3[String, A, PatternList[A]].contramap(pat => (Tag, pat.attributes, pat.elementPatterns))

    implicit def decodeTuplePattern[A: Decoder]: Decoder[TuplePattern[A]] =
      Decoder
        .decodeTuple3[String, A, PatternList[A]]
        .ensure(hasMatchingTag, s"""The tag of a tuple pattern must be "$Tag".""")
        .map {
          case (_, attributes, elementPatterns) => TuplePattern(attributes, elementPatterns)
        }
  }

  final case class RecordPattern[+A](attributes: A, fieldNames: List[Name]) extends Pattern[A] {
    def mapAttributes[B](f: A => B): Pattern[B] = RecordPattern(f(attributes), fieldNames)
  }

  object RecordPattern extends TaggedCompanionObject("record_pattern") {
    implicit def encodeRecordPattern[A: Encoder]: Encoder[RecordPattern[A]] =
      Encoder.encodeTuple3[String, A, List[Name]].contramap(pat => (Tag, pat.attributes, pat.fieldNames))

    implicit def decodeRecordPattern[A: Decoder]: Decoder[RecordPattern[A]] =
      Decoder
        .decodeTuple3[String, A, List[Name]]
        .ensure(hasMatchingTag, s"""The tag of a record pattern must be "$Tag".""")
        .map {
          case (_, attributes, fieldNames) => RecordPattern(attributes, fieldNames)
        }
  }

  final case class ConstructorPattern[+A](attributes: A, constructorName: FQName, argumentPatterns: PatternList[A])
      extends Pattern[A] {
    def mapAttributes[B](f: A => B): Pattern[B] =
      ConstructorPattern(f(attributes), constructorName, argumentPatterns.mapAttributes(f))
  }

  object ConstructorPattern extends TaggedCompanionObject("constructor_pattern") {

    implicit def encodeConstructorPattern[A: Encoder]: Encoder[ConstructorPattern[A]] =
      Encoder
        .encodeTuple4[String, A, FQName, PatternList[A]]
        .contramap(pat => (Tag, pat.attributes, pat.constructorName, pat.argumentPatterns))

    implicit def decodeConstructorPattern[A: Decoder]: Decoder[ConstructorPattern[A]] =
      Decoder
        .decodeTuple4[String, A, FQName, PatternList[A]]
        .ensure(hasMatchingTag, s"""The tag of a constructor pattern must be "$Tag".""")
        .map {
          case (_, attributes, constructorName, argumentPatterns) =>
            ConstructorPattern(attributes, constructorName, argumentPatterns)
        }
  }

  final case class EmptyListPattern[+A](attributes: A) extends Pattern[A] {
    def mapAttributes[B](f: A => B): Pattern[B] = EmptyListPattern(f(attributes))
  }

  object EmptyListPattern extends TaggedCompanionObject("empty_list_pattern") {
    implicit def encodeEmptyListPattern[A: Encoder]: Encoder[EmptyListPattern[A]] =
      Encoder.encodeTuple2[String, A].contramap(p => (Tag, p.attributes))

    implicit def decodeEmptyListPattern[A: Decoder]: Decoder[EmptyListPattern[A]] =
      Decoder
        .decodeTuple2[String, A]
        .ensure(hasMatchingTag, s"""The tag of a empty list pattern must be "$Tag".""")
        .map {
          case (_, attributes) => EmptyListPattern(attributes)
        }
  }

  final case class HeadTailPattern[+A](attributes: A, headPattern: Pattern[A], tailPattern: Pattern[A])
      extends Pattern[A] {
    def mapAttributes[B](f: A => B): Pattern[B] =
      HeadTailPattern(f(attributes), headPattern.mapAttributes(f), tailPattern.mapAttributes(f))
  }

  object HeadTailPattern extends TaggedCompanionObject("head_tail_pattern") {
    implicit def encodeHeadTailPattern[A: Encoder]: Encoder[HeadTailPattern[A]] =
      Encoder
        .encodeTuple4[String, A, Pattern[A], Pattern[A]]
        .contramap(p => (Tag, p.attributes, p.headPattern, p.tailPattern))

    implicit def decodeHeadTailPattern[A: Decoder]: Decoder[HeadTailPattern[A]] =
      Decoder
        .decodeTuple4[String, A, Pattern[A], Pattern[A]]
        .ensure(hasMatchingTag, s"""The tag of a head tail pattern must be "$Tag".""")
        .map {
          case (_, attributes, headPattern, tailPattern) => HeadTailPattern(attributes, headPattern, tailPattern)
        }
  }

  final case class LiteralPattern[+A](attributes: A, value: LiteralValue) extends Pattern[A] {
    def mapAttributes[B](f: A => B): Pattern[B] = LiteralPattern(f(attributes), value)
  }

  object LiteralPattern extends TaggedCompanionObject("literal_pattern") {
    implicit def encodeLiteralPattern[A: Encoder]: Encoder[LiteralPattern[A]] =
      Encoder.encodeTuple3[String, A, LiteralValue].contramap(p => (Tag, p.attributes, p.value))

    implicit def decodeLiteralPattern[A: Decoder]: Decoder[LiteralPattern[A]] =
      Decoder
        .decodeTuple3[String, A, LiteralValue]
        .ensure(hasMatchingTag, s"""The tag of a literal pattern must be "$Tag".""")
        .map {
          case (_, attributes, value) => LiteralPattern(attributes, value)
        }
  }

  final case class UnitPattern[+A](attributes: A) extends Pattern[A] {
    def mapAttributes[B](f: A => B): Pattern[B] = UnitPattern(f(attributes))
  }

  object UnitPattern extends TaggedCompanionObject("unit_pattern") {
    implicit def encodeUnitPattern[A: Encoder]: Encoder[UnitPattern[A]] =
      Encoder.encodeTuple2[String, A].contramap(p => (Tag, p.attributes))

    implicit def decodeUnitPattern[A: Decoder]: Decoder[UnitPattern[A]] =
      Decoder
        .decodeTuple2[String, A]
        .ensure(hasMatchingTag, s"""The tag of a unit pattern must be "$Tag".""")
        .map {
          case (_, attributes) => UnitPattern(attributes)
        }
  }

  implicit def encodePattern[A: Encoder]: Encoder[Pattern[A]] = Encoder.instance {
    case p @ WildcardPattern(_)          => p.asJson
    case p @ AsPattern(_, _, _)          => p.asJson
    case p @ TuplePattern(_, _)          => p.asJson
    case p @ RecordPattern(_, _)         => p.asJson
    case p @ ConstructorPattern(_, _, _) => p.asJson
    case p @ EmptyListPattern(_)         => p.asJson
    case p @ HeadTailPattern(_, _, _)    => p.asJson
    case p @ LiteralPattern(_, _)        => p.asJson
    case p @ UnitPattern(_)              => p.asJson
  }

  implicit def decodePattern[A: Decoder]: Decoder[Pattern[A]] =
    Decoder[WildcardPattern[A]]
      .widen[Pattern[A]]
      .or(Decoder[AsPattern[A]].widen)
      .or(Decoder[TuplePattern[A]].widen)
      .or(Decoder[RecordPattern[A]].widen)
      .or(Decoder[ConstructorPattern[A]].widen)
      .or(Decoder[EmptyListPattern[A]].widen)
      .or(Decoder[HeadTailPattern[A]].widen)
      .or(Decoder[LiteralPattern[A]].widen)
      .or(Decoder[UnitPattern[A]].widen)
}
