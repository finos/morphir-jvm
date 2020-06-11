package morphir.ir.codec

import morphir.ir.FQName
import morphir.ir.core.TaggedCompanionObjectLike
import morphir.ir.json.Decode.DecodeError
import morphir.ir.literal
import morphir.ir.name.Name
import morphir.ir.pattern.{ Pattern, PatternList }
import upickle.default._

object patternCodecs {
  trait PatternCodec extends TaggedCompanionObjectLike {
    val Tag: String = "pattern"

    implicit def readWriter[A: ReadWriter](
      implicit literalReadWriter: ReadWriter[literal.Literal],
      patternListReadWriter: ReadWriter[PatternList[A]]
    ): ReadWriter[Pattern[A]] =
      readwriter[ujson.Value].bimap(
        {
          case pat @ Pattern.WildcardPattern(_)          => writeJs(pat)
          case pat @ Pattern.AsPattern(_, _, _)          => writeJs(pat)
          case pat @ Pattern.TuplePattern(_, _)          => writeJs(pat)
          case pat @ Pattern.RecordPattern(_, _)         => writeJs(pat)
          case pat @ Pattern.ConstructorPattern(_, _, _) => writeJs(pat)
          case pat @ Pattern.EmptyListPattern(_)         => writeJs(pat)
          case pat @ Pattern.HeadTailPattern(_, _, _)    => writeJs(pat)
          case pat @ Pattern.LiteralPattern(_, _)        => writeJs(pat)
          case pat @ Pattern.UnitPattern(_)              => writeJs(pat)
        },
        json => {
          val tagName = json(0).str
          tagName match {
            case tag if tag == Pattern.WildcardPattern.Tag    => read[Pattern.WildcardPattern[A]](json)
            case tag if tag == Pattern.AsPattern.Tag          => read[Pattern.AsPattern[A]](json)
            case tag if tag == Pattern.TuplePattern.Tag       => read[Pattern.TuplePattern[A]](json)
            case tag if tag == Pattern.RecordPattern.Tag      => read[Pattern.RecordPattern[A]](json)
            case tag if tag == Pattern.ConstructorPattern.Tag => read[Pattern.ConstructorPattern[A]](json)
            case tag if tag == Pattern.EmptyListPattern.Tag   => read[Pattern.EmptyListPattern[A]](json)
            case tag if tag == Pattern.HeadTailPattern.Tag    => read[Pattern.HeadTailPattern[A]](json)
            case tag if tag == Pattern.LiteralPattern.Tag     => read[Pattern.LiteralPattern[A]](json)
            case tag if tag == Pattern.UnitPattern.Tag        => read[Pattern.UnitPattern[A]](json)
            case tag                                          => throw DecodeError.unexpectedTag(tag, List.empty)
          }
        }
      )
  }

  trait WildcardPatternCodec extends TaggedCompanionObjectLike {
    val Tag: String = "wildcard_pattern"

    implicit def readWriter[A: ReadWriter]: ReadWriter[Pattern.WildcardPattern[A]] =
      readwriter[(String, A)].bimap(
        pat => (Tag, pat.attributes), {
          case (tag, attributes) if tag == Tag => Pattern.WildcardPattern(attributes)
          case (tag, _)                        => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )
  }

  trait AsPatternCodec extends TaggedCompanionObjectLike {
    val Tag: String = "as_pattern"

    implicit def readWriter[A: ReadWriter](
      implicit patternReadWriter: ReadWriter[Pattern[A]]
    ): ReadWriter[Pattern.AsPattern[A]] =
      readwriter[(String, A, Pattern[A], Name)].bimap[Pattern.AsPattern[A]](
        pat => (Tag, pat.attributes, pat.pattern, pat.name), {
          case (tag, attributes, pattern, name) if tag == Tag => Pattern.AsPattern(attributes, pattern, name)
          case (tag, _, _, _)                                 => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )
  }

  trait TuplePatternCodec extends TaggedCompanionObjectLike {
    val Tag: String = "tuple_pattern"

    implicit def readWriter[A: ReadWriter]: ReadWriter[Pattern.TuplePattern[A]] =
      readwriter[ujson.Value].bimap(pat => ujson.Arr(ujson.Str(Tag), writeJs(pat.attributes)), _ => ???)
    //      readwriter[(String, A, List[Pattern[A]])].bimap(
//        pat => (Tag, pat.attributes, pat.elementPatterns), {
//          case (tag, attributes, elementPatterns) if tag == Tag => Pattern.TuplePattern(attributes, elementPatterns)
//          case (tag, _, _)                                      => throw DecodeError.unexpectedTag(tag, Tag)
//        }
//      )
  }

  trait RecordPatternCodec extends TaggedCompanionObjectLike {
    val Tag: String = "record_pattern"

    implicit def readWriter[A: ReadWriter]: ReadWriter[Pattern.RecordPattern[A]] =
      readwriter[(String, A, List[Name])].bimap(
        pat => (Tag, pat.attributes, pat.fieldNames), {
          case (tag, attributes, fieldNames) if tag == Tag => Pattern.RecordPattern(attributes, fieldNames)
          case (tag, _, _)                                 => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )
  }

  trait ConstructorPatternCodec extends TaggedCompanionObjectLike {
    val Tag: String = "constructor_pattern"

    implicit def readWriter[A: ReadWriter](
      implicit patternListReadWriter: ReadWriter[PatternList[A]]
    ): ReadWriter[Pattern.ConstructorPattern[A]] =
      readwriter[(String, A, FQName, PatternList[A])]
        .bimap[Pattern.ConstructorPattern[A]](
          pat => (Tag, pat.attributes, pat.constructorName, pat.argumentPatterns), {
            case (tag: String, attributes, constructorName: FQName, argumentPatterns) if tag == Tag =>
              Pattern.ConstructorPattern(attributes, constructorName, argumentPatterns)
            case (tag: String, _, _, _) => throw DecodeError.unexpectedTag(tag, Tag)
          }
        )
  }

  trait EmptyListPatternCodec extends TaggedCompanionObjectLike {
    val Tag: String = "empty_list_pattern"

    implicit def readWriter[A: ReadWriter]: ReadWriter[Pattern.EmptyListPattern[A]] =
      readwriter[(String, A)].bimap(
        pat => (Tag, pat.attributes), {
          case (tag, attributes) if tag == Tag => Pattern.EmptyListPattern(attributes)
          case (tag, _)                        => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )
  }

  trait HeadTailPatternCodec extends TaggedCompanionObjectLike {
    val Tag: String = "head_tail_pattern"

    implicit def readWriter[A: ReadWriter](
      implicit patternReadWriter: ReadWriter[Pattern[A]]
    ): ReadWriter[Pattern.HeadTailPattern[A]] =
      readwriter[(String, A, Pattern[A], Pattern[A])].bimap(
        pat => (Tag, pat.attributes, pat.headPattern, pat.tailPattern), {
          case (tag, attributes, headPattern, tailPattern) if tag == Tag =>
            Pattern.HeadTailPattern(attributes, headPattern, tailPattern)
          case (tag, _, _, _) => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )
  }

  trait LiteralPattenCodec extends TaggedCompanionObjectLike {
    val Tag: String = "literal_pattern"

    implicit def readWriter[A: ReadWriter](
      implicit literalReadWriter: ReadWriter[literal.Literal]
    ): ReadWriter[Pattern.LiteralPattern[A]] =
      readwriter[(String, A, literal.Literal)].bimap[Pattern.LiteralPattern[A]](
        pat => (Tag, pat.attributes, pat.value), {
          case (tag, attributes, value: literal.Literal) if tag == Tag => Pattern.LiteralPattern(attributes, value)
          case (tag: String, _, _)                                     => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )
  }

  trait UnitPatternCodec extends TaggedCompanionObjectLike {
    val Tag: String = "unit_pattern"

    implicit def readWriter[A: ReadWriter]: ReadWriter[Pattern.UnitPattern[A]] =
      readwriter[(String, A)].bimap(
        pat => (Tag, pat.attributes), {
          case (tag, attributes) if tag == Tag => Pattern.UnitPattern(attributes)
          case (tag: String, _)                => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )
  }
}
