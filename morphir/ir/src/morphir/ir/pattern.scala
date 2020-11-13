/*
Copyright 2020 Morgan Stanley

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 */

package morphir.ir

import morphir.ir.codec.patternCodecs

object pattern {
  implicit class PatternListOps[+A](private val self: List[Pattern[A]]) extends AnyVal {
    def mapAttributes[B](f: A => B): List[Pattern[B]] =
      self.map(pat => pat.mapAttributes(f))
  }

  sealed abstract class Pattern[+A] extends Product with Serializable {
    def attributes: A

    def mapAttributes[B](f: A => B): Pattern[B]
  }

  object Pattern extends patternCodecs.PatternCodec {

    final case class WildcardPattern[+A](attributes: A) extends Pattern[A] {
      def mapAttributes[B](f: A => B): Pattern[B] = WildcardPattern(f(attributes))
    }

    object WildcardPattern extends patternCodecs.WildcardPatternCodec

    final case class AsPattern[+A](attributes: A, pattern: Pattern[A], name: Name) extends Pattern[A] {
      def mapAttributes[B](f: A => B): Pattern[B] = AsPattern(f(attributes), pattern mapAttributes f, name)
    }
    object AsPattern                                                               extends patternCodecs.AsPatternCodec

    final case class TuplePattern[+A](attributes: A, elementPatterns: List[Pattern[A]]) extends Pattern[A] {
      def mapAttributes[B](f: A => B): Pattern[B] = TuplePattern(f(attributes), elementPatterns.mapAttributes(f))
    }

    object TuplePattern extends patternCodecs.TuplePatternCodec

    final case class RecordPattern[+A](attributes: A, fieldNames: List[Name]) extends Pattern[A] {
      def mapAttributes[B](f: A => B): Pattern[B] = RecordPattern(f(attributes), fieldNames)
    }

    object RecordPattern extends patternCodecs.RecordPatternCodec

    final case class ConstructorPattern[+A](attributes: A, constructorName: FQName, argumentPatterns: List[Pattern[A]])
        extends Pattern[A] {
      def mapAttributes[B](f: A => B): Pattern[B] =
        ConstructorPattern(f(attributes), constructorName, argumentPatterns.mapAttributes(f))
    }

    object ConstructorPattern extends patternCodecs.ConstructorPatternCodec

    final case class EmptyListPattern[+A](attributes: A) extends Pattern[A] {
      def mapAttributes[B](f: A => B): Pattern[B] = EmptyListPattern(f(attributes))
    }

    object EmptyListPattern extends patternCodecs.EmptyListPatternCodec

    final case class HeadTailPattern[+A](attributes: A, headPattern: Pattern[A], tailPattern: Pattern[A])
        extends Pattern[A] {
      def mapAttributes[B](f: A => B): Pattern[B] =
        HeadTailPattern(f(attributes), headPattern.mapAttributes(f), tailPattern.mapAttributes(f))
    }

    object HeadTailPattern extends patternCodecs.HeadTailPatternCodec

    final case class LiteralPattern[+A](attributes: A, value: literal.Literal) extends Pattern[A] {
      def mapAttributes[B](f: A => B): Pattern[B] = LiteralPattern(f(attributes), value)
    }

    object LiteralPattern extends patternCodecs.LiteralPattenCodec

    final case class UnitPattern[+A](attributes: A) extends Pattern[A] {
      def mapAttributes[B](f: A => B): Pattern[B] = UnitPattern(f(attributes))
    }

    object UnitPattern extends patternCodecs.UnitPatternCodec
  }

  case class PatternMatchCase[+A](pattern: Pattern[A], value: Value[A]) {

    def mapAttributes[B](f: A => B): PatternMatchCase[B] =
      PatternMatchCase(pattern.mapAttributes(f), value.mapAttributes(f))

    @inline def toTuple: (Pattern[A], Value[A]) = pattern -> value
  }
}
