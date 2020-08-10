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


package org.morphir.sdk.json

import org.morphir.internal.collection.decorators._
import org.morphir.sdk.core.{Maybe, Result}
import org.morphir.sdk.core.Result.{Err, Ok}
import org.morphir.sdk.core.Maybe._
import org.morphir.sdk.core.Dict

import scala.util.control.NonFatal
import scala.reflect.ClassTag
import scala.collection.mutable
import cats.syntax.either._
import io.circe._
import io.circe.parser._

object Decode {

  type Value = Encode.Value
  type DecodeResult[A] = Result[Error, A]

  def parse(input: String): DecodeResult[Json] =
    parser.parse(input) match {
      case Right(value) => Result.ok(value)
      case Left(failure) =>
        Result.err(Error.Failure(failure.message, Json.fromString(input)))
    }

  def decodeString[A](decoder: Decoder[A])(input: String): DecodeResult[A] =
    parse(input).flatMap(decodeValue(decoder)(_))

  def decodeValue[A](decoder: Decoder[A])(jsonValue: Value): DecodeResult[A] = {
    decoder.decodeValue(jsonValue)
  }

  def succeed[A](value: => A): Decoder[A] =
    Decoder.Succeed(value)

  def fail[A](message: => String): Decoder[A] =
    Decoder.Fail(message)

  def andThen[A, B](fn: A => Decoder[B])(decoder: Decoder[A]): Decoder[B] =
    Decoder.AndThen(fn, decoder)

  def `lazy`[A](thunk: () => Decoder[A]): Decoder[A] =
    andThen((_: Unit) => thunk())(succeed(()))

  @inline def lzy[A](thunk: () => Decoder[A]): Decoder[A] = `lazy`(thunk)

  /** Decode a `null` value into some actual value */
  def `null`[A](value: => A): Decoder[A] =
    Decoder.Null(value)

  /** Decode a `null` value into some actual value */
  def Null[A](value: => A): Decoder[A] =
    Decoder.Null(value)

  def value(jsonValue: Value): Decoder[Value] =
    Decoder.Value(jsonValue)

  val string: Decoder[String] = Decoder.String

  val bool: Decoder[Boolean] = Decoder.Bool

  val int: Decoder[Int] = Decoder.Int

  val float: Decoder[Float] = Decoder.Float

  def field[A](field: String)(decoder: Decoder[A]): Decoder[A] =
    Decoder.Field(field, decoder)

  def decodeField[A](field: String, decoder: Decoder[A]): Decoder[A] =
    Decoder.Field(field, decoder)

  def at[A](fields: String*)(decoder: Decoder[A]): Decoder[A] =
    at(fields.toList)(decoder)

  def at[A](fields: List[String])(decoder: Decoder[A]): Decoder[A] =
    fields.foldRight(decoder)((fld, fieldDecoder) => field(fld)(fieldDecoder))

  def index[A](idx: Int)(decoder: Decoder[A]): Decoder[A] =
    Decoder.Index(idx, decoder)

  def oneOf[A](decoders: List[Decoder[A]]): Decoder[A] =
    Decoder.OneOf(decoders)

  def oneOf[A](
      firstDecoder: Decoder[A],
      otherDecoders: Decoder[A]*
  ): Decoder[A] =
    Decoder.OneOf(List(firstDecoder) ++ otherDecoders)

  def nullable[A](decoder: Decoder[A]): Decoder[Maybe[A]] =
    oneOf(`null`(Maybe.Nothing), map((a: A) => Maybe.just(a))(decoder))

  def list[A](decoder: Decoder[A]): Decoder[List[A]] =
    Decoder.ListDecoder(decoder)

  def array[A: ClassTag](decoder: Decoder[A]): Decoder[Array[A]] =
    Decoder.ArrayDecoder(decoder)

  def dict[A](decoder: Decoder[A]): Decoder[Dict.Dict[String, A]] =
    map(Dict.fromList[String, A])(keyValuePairs((decoder)))

  def keyValuePairs[A](decoder: Decoder[A]): Decoder[List[(String, A)]] =
    Decoder.KeyValuePairs(decoder)

  def oneOrMore[A, V](
      toValue: A => List[A] => V
  )(decoder: Decoder[A]): Decoder[V] = {

    andThen(oneOrMoreHelp(toValue))(list(decoder))
  }

  private def oneOrMoreHelp[A, V](
      toValue: A => List[A] => V
  )(xs: List[A]): Decoder[V] =
    xs match {
      case Nil     => fail("an ARRAY with at least ONE element")
      case y :: ys => succeed(toValue(y)(ys))
    }

  def maybe[A](decoder: Decoder[A]): Decoder[Maybe[A]] =
    oneOf(map((a: A) => Maybe.just(a))(decoder), Decode.succeed(Maybe.Nothing))

  def map[A, V](fn: A => V)(decoder: Decoder[A]): Decoder[V] =
    Decoder.Map(fn, decoder)

  def map2[A, B, V](
      fn: A => B => V
  )(decoderA: Decoder[A])(decoderB: Decoder[B]): Decoder[V] = {
    def f(a: A, b: B) = fn(a)(b)
    Decoder.Map2(f, decoderA, decoderB)
  }

  def map2[A, B, V](
      fn: (A, B) => V
  )(decoderA: Decoder[A])(decoderB: Decoder[B]): Decoder[V] = {
    Decoder.Map2(fn, decoderA, decoderB)
  }

  def map3[A, B, C, V](
      fn: A => B => C => V
  )(
      decoderA: Decoder[A]
  )(decoderB: Decoder[B])(decoderC: Decoder[C]): Decoder[V] = {
    def f(a: A, b: B, c: C) = fn(a)(b)(c)
    Decoder.Map3(f, decoderA, decoderB, decoderC)
  }

  def map3[A, B, C, V](
      fn: (A, B, C) => V
  )(
      decoderA: Decoder[A]
  )(decoderB: Decoder[B])(decoderC: Decoder[C]): Decoder[V] = {
    Decoder.Map3(fn, decoderA, decoderB, decoderC)
  }

  def map4[A, B, C, D, V](
      fn: A => B => C => D => V
  )(
      decoderA: Decoder[A]
  )(
      decoderB: Decoder[B]
  )(decoderC: Decoder[C])(decoderD: Decoder[D]): Decoder[V] = {
    def f(a: A, b: B, c: C, d: D) = fn(a)(b)(c)(d)
    Decoder.Map4(f, decoderA, decoderB, decoderC, decoderD)
  }

  def map4[A, B, C, D, V](
      fn: (A, B, C, D) => V
  )(
      decoderA: Decoder[A]
  )(
      decoderB: Decoder[B]
  )(decoderC: Decoder[C])(decoderD: Decoder[D]): Decoder[V] = {
    Decoder.Map4(fn, decoderA, decoderB, decoderC, decoderD)
  }

  def map5[A, B, C, D, E, V](
      fn: A => B => C => D => E => V
  )(
      decoderA: Decoder[A]
  )(
      decoderB: Decoder[B]
  )(
      decoderC: Decoder[C]
  )(decoderD: Decoder[D])(decoderE: Decoder[E]): Decoder[V] = {
    def f(a: A, b: B, c: C, d: D, e: E) = fn(a)(b)(c)(d)(e)
    Decoder.Map5(f, decoderA, decoderB, decoderC, decoderD, decoderE)
  }

  def map5[A, B, C, D, E, V](
      fn: (A, B, C, D, E) => V
  )(
      decoderA: Decoder[A]
  )(
      decoderB: Decoder[B]
  )(
      decoderC: Decoder[C]
  )(decoderD: Decoder[D])(decoderE: Decoder[E]): Decoder[V] = {
    Decoder.Map5(fn, decoderA, decoderB, decoderC, decoderD, decoderE)
  }

  def map6[A, B, C, D, E, F, V](
      fn: A => B => C => D => E => F => V
  )(
      decoderA: Decoder[A]
  )(
      decoderB: Decoder[B]
  )(
      decoderC: Decoder[C]
  )(
      decoderD: Decoder[D]
  )(decoderE: Decoder[E])(decoderF: Decoder[F]): Decoder[V] = {
    def f(a: A, b: B, c: C, d: D, e: E, f: F) = fn(a)(b)(c)(d)(e)(f)
    Decoder.Map6(f, decoderA, decoderB, decoderC, decoderD, decoderE, decoderF)
  }

  def map6[A, B, C, D, E, F, V](
      fn: (A, B, C, D, E, F) => V
  )(
      decoderA: Decoder[A]
  )(
      decoderB: Decoder[B]
  )(
      decoderC: Decoder[C]
  )(
      decoderD: Decoder[D]
  )(decoderE: Decoder[E])(decoderF: Decoder[F]): Decoder[V] = {
    Decoder.Map6(fn, decoderA, decoderB, decoderC, decoderD, decoderE, decoderF)
  }

  def map7[A, B, C, D, E, F, G, V](
      fn: A => B => C => D => E => F => G => V
  )(
      decoderA: Decoder[A]
  )(
      decoderB: Decoder[B]
  )(
      decoderC: Decoder[C]
  )(
      decoderD: Decoder[D]
  )(
      decoderE: Decoder[E]
  )(decoderF: Decoder[F])(decoderG: Decoder[G]): Decoder[V] = {
    def f(a: A, b: B, c: C, d: D, e: E, f: F, g: G) = fn(a)(b)(c)(d)(e)(f)(g)
    Decoder.Map7(
      f,
      decoderA,
      decoderB,
      decoderC,
      decoderD,
      decoderE,
      decoderF,
      decoderG
    )
  }

  def map7[A, B, C, D, E, F, G, V](
      fn: (A, B, C, D, E, F, G) => V
  )(
      decoderA: Decoder[A]
  )(
      decoderB: Decoder[B]
  )(
      decoderC: Decoder[C]
  )(
      decoderD: Decoder[D]
  )(
      decoderE: Decoder[E]
  )(decoderF: Decoder[F])(decoderG: Decoder[G]): Decoder[V] = {
    Decoder.Map7(
      fn,
      decoderA,
      decoderB,
      decoderC,
      decoderD,
      decoderE,
      decoderF,
      decoderG
    )
  }

  def map8[A, B, C, D, E, F, G, H, V](
      fn: A => B => C => D => E => F => G => H => V
  )(
      decoderA: Decoder[A]
  )(
      decoderB: Decoder[B]
  )(
      decoderC: Decoder[C]
  )(
      decoderD: Decoder[D]
  )(
      decoderE: Decoder[E]
  )(
      decoderF: Decoder[F]
  )(decoderG: Decoder[G])(decoderH: Decoder[H]): Decoder[V] = {
    def f(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H) =
      fn(a)(b)(c)(d)(e)(f)(g)(h)
    Decoder.Map8(
      f,
      decoderA,
      decoderB,
      decoderC,
      decoderD,
      decoderE,
      decoderF,
      decoderG,
      decoderH
    )
  }

  def map7[A, B, C, D, E, F, G, H, V](
      fn: (A, B, C, D, E, F, G, H) => V
  )(
      decoderA: Decoder[A]
  )(
      decoderB: Decoder[B]
  )(
      decoderC: Decoder[C]
  )(
      decoderD: Decoder[D]
  )(
      decoderE: Decoder[E]
  )(
      decoderF: Decoder[F]
  )(decoderG: Decoder[G])(decoderH: Decoder[H]): Decoder[V] = {
    Decoder.Map8(
      fn,
      decoderA,
      decoderB,
      decoderC,
      decoderD,
      decoderE,
      decoderF,
      decoderG,
      decoderH
    )
  }

  object DecodeResult {
    def ok[A](value: => A): DecodeResult[A] =
      Result.Ok(value)

    def err[A](err: => Error): DecodeResult[A] =
      Result.Err(err)

    def errorExpecting(expectationDescription: String, value: Value) =
      Result.Err(Error.Failure(s"Expecting $expectationDescription", value))
  }

  sealed abstract class Decoder[A] {
    def decodeValue(jsonValue: Decode.Value): DecodeResult[A]
  }
  object Decoder {
    private[Decode] case class Succeed[A](value: A) extends Decoder[A] {

      def decodeValue(
          jsonValue: org.morphir.sdk.json.Decode.Value
      ): DecodeResult[A] =
        Result.Ok(value)

    }
    private[Decode] case class Fail[A](message: String) extends Decoder[A] {
      def decodeValue(
          jsonValue: org.morphir.sdk.json.Decode.Value
      ): DecodeResult[A] =
        Result.Err(Error.Failure(message, jsonValue))
    }
    private[Decode] case class Null[A](value: A) extends Decoder[A] {
      def decodeValue(
          jsonValue: org.morphir.sdk.json.Decode.Value
      ): DecodeResult[A] =
        if (jsonValue.isNull) Result.Ok(value)
        else DecodeResult.errorExpecting("null", jsonValue)
    }

    private[Decode] case object String extends Decoder[scala.Predef.String] {
      def decodeValue(
          jsonValue: org.morphir.sdk.json.Decode.Value
      ): DecodeResult[String] =
        jsonValue match {
          case Json.JString(value) => DecodeResult.ok(value)
          case _                   => DecodeResult.errorExpecting("a STRING", jsonValue)
        }
    }
    private[Decode] case object Bool extends Decoder[Boolean] {
      def decodeValue(
          jsonValue: org.morphir.sdk.json.Decode.Value
      ): DecodeResult[Boolean] =
        jsonValue match {
          case Json.JBoolean(value) => DecodeResult.ok(value)
          case _                    => DecodeResult.errorExpecting("a BOOL", jsonValue)
        }
    }
    private[Decode] case object Int extends Decoder[Int] {
      def decodeValue(
          jsonValue: org.morphir.sdk.json.Decode.Value
      ): DecodeResult[Int] = {
        lazy val error = DecodeResult.errorExpecting("an INT", jsonValue)
        jsonValue match {
          case Json.JNumber(value) =>
            val number = value.toDouble
            if (scala.Int.MinValue <= number && number <= scala.Int.MaxValue && number.isWhole)
              DecodeResult.ok(number.toInt)
            else
              error
          case _ => error
        }
      }
    }

    private[Decode] case object Float extends Decoder[Float] {
      def decodeValue(
          jsonValue: org.morphir.sdk.json.Decode.Value
      ): DecodeResult[Float] = {

        lazy val error = DecodeResult.errorExpecting("a FLOAT", jsonValue)

        jsonValue match {
          case Json.JNumber(value) =>
            val number = value.toDouble
            if (scala.Float.MinValue <= number && number <= scala.Float.MaxValue)
              DecodeResult.ok(number.toFloat)
            else
              error
          case _ => error
        }
      }
    }

    private[Decode] case class AndThen[A, B](
        fn: A => Decoder[B],
        decoder: Decoder[A]
    ) extends Decoder[B] {

      def decodeValue(
          jsonValue: org.morphir.sdk.json.Decode.Value
      ): DecodeResult[B] =
        decoder
          .decodeValue(jsonValue)
          .flatMap(a => fn(a).decodeValue(jsonValue))

    }

    private[Decode] case class OneOf[A](decoders: List[Decoder[A]])
        extends Decoder[A] {
      def decodeValue(
          jsonValue: org.morphir.sdk.json.Decode.Value
      ): DecodeResult[A] =
        decoders
          .foldLeft[Result[Error, A]](Result.Err(Error.OneOf(Nil))) {
            case (res @ Ok(_), _) => res
            case (Err(error), decoder) =>
              val decodeResult = Decode.decodeValue(decoder)(jsonValue)
              (error, decodeResult) match {
                case (_, Ok(_)) => decodeResult
                case (Error.OneOf(errors), Err(decodeError)) =>
                  Err(Error.OneOf(decodeError :: errors))
                case (_, Err(decodeError)) =>
                  Err(Error.OneOf(decodeError :: error :: Nil))
              }

          }
          .mapError {
            case Error.OneOf(errors) => Error.oneOf(errors.reverse)
            case err                 => err
          }
    }
    private[Decode] case class Value(jsonValue: Decode.Value)
        extends Decoder[Decode.Value] {
      def decodeValue(
          jsonValue: org.morphir.sdk.json.Decode.Value
      ): DecodeResult[org.morphir.sdk.json.Decode.Value] =
        Result.Ok(jsonValue)
    }

    private[Decode] case class Field[A](field: String, decoder: Decoder[A])
        extends Decoder[A] {
      def decodeValue(
          jsonValue: org.morphir.sdk.json.Decode.Value
      ): DecodeResult[A] = {
        lazy val error = DecodeResult.errorExpecting(
          s"an OBJECT with a field named '$field'",
          jsonValue
        )
        jsonValue match {
          case Json.JObject(fields) if fields.contains(field) =>
            val fieldValue: Option[Decode.Value] = fields(field)
            fieldValue match {
              case None         => error
              case Some(result) => decoder.decodeValue(result)
            }
          case _ => error
        }
      }
    }

    private[Decode] case class Index[A](index: Int, decoder: Decoder[A])
        extends Decoder[A] {
      def decodeValue(
          jsonValue: org.morphir.sdk.json.Decode.Value
      ): DecodeResult[A] = {
        jsonValue match {
          case Arr(items) if index >= items.length =>
            DecodeResult.errorExpecting(
              s"a LONGER array. Need index '$index' but only see '${items.length}' enties",
              jsonValue
            )
          case Arr(items) =>
            decoder.decodeValue(items(index)) match {
              case Err(error) => DecodeResult.err(Error.Index(index, error))
              case res        => res
            }
          case _ => DecodeResult.errorExpecting("an ARRAY", jsonValue)
        }
      }
    }

    private[Decode] case class ListDecoder[A](decoder: Decoder[A])
        extends Decoder[List[A]] {
      def decodeValue(
          jsonValue: org.morphir.sdk.json.Decode.Value
      ): DecodeResult[List[A]] = {
        jsonValue match {
          case ujson.Arr(elements) =>
            elements
              .foldSomeLeft(DecodeResult.ok(List.empty[A])) {
                case (result, jsonValue) =>
                  result match {
                    case Err(_) => None
                    case Ok(elements) =>
                      decoder.decodeValue(jsonValue) match {
                        case Ok(element) =>
                          Some(DecodeResult.ok(element :: elements))
                        case Err(error) =>
                          Some(
                            DecodeResult
                              .err(Error.Index(elements.length, error))
                          )
                      }
                  }
              }
              .map(_.reverse)
          case _ =>
            DecodeResult.errorExpecting("a LIST", jsonValue)
        }
      }
    }

    private[Decode] case class ArrayDecoder[A: ClassTag](decoder: Decoder[A])
        extends Decoder[Array[A]] {
      def decodeValue(
          jsonValue: org.morphir.sdk.json.Decode.Value
      ): DecodeResult[Array[A]] = {
        jsonValue match {
          case ujson.Arr(elements) =>
            elements
              .foldSomeLeft(DecodeResult.ok(mutable.ArrayBuffer.empty[A])) {
                case (result, jsonValue) =>
                  result match {
                    case Err(_) => None
                    case Ok(elements) =>
                      decoder.decodeValue(jsonValue) match {
                        case Ok(element) =>
                          Some(DecodeResult.ok(elements += element))
                        case Err(error) =>
                          Some(
                            DecodeResult
                              .err(Error.Index(elements.length, error))
                          )
                      }
                  }
              }
              .map(_.toArray)
          case _ =>
            DecodeResult.errorExpecting("an ARRAY", jsonValue)
        }
      }
    }

    private[Decode] case class KeyValuePairs[A](decoder: Decoder[A])
        extends Decoder[List[(String, A)]] {
      def decodeValue(
          jsonValue: org.morphir.sdk.json.Decode.Value
      ): DecodeResult[List[(String, A)]] = {
        jsonValue match {
          case ujson.Obj(items) =>
            items.toSeq
              .foldSomeLeft(DecodeResult.ok(List.empty[(String, A)])) {
                case (result, (key, value)) =>
                  result match {
                    case Err(_) => None
                    case Ok(elements) =>
                      decoder.decodeValue(value) match {
                        case Ok(element) =>
                          Some(DecodeResult.ok((key -> element) :: elements))
                        case Err(error) =>
                          Some(
                            DecodeResult
                              .err(Error.Index(elements.length, error))
                          )
                      }
                  }
              }
              .map(_.reverse)
          case _ => DecodeResult.errorExpecting("an OBJECT", jsonValue)
        }
      }
    }

    private[Decode] case class Map[A, V](fn: A => V, decoder: Decoder[A])
        extends Decoder[V] {
      def decodeValue(
          jsonValue: org.morphir.sdk.json.Decode.Value
      ): DecodeResult[V] =
        decoder.decodeValue(jsonValue).map(fn)
    }
    private[Decode] case class Map2[A, B, V](
        fn: (A, B) => V,
        decoderA: Decoder[A],
        decoderB: Decoder[B]
    ) extends Decoder[V] {
      def decodeValue(
          jsonValue: org.morphir.sdk.json.Decode.Value
      ): DecodeResult[V] = (
        for {
          a <- decoderA.decodeValue(jsonValue)
          b <- decoderB.decodeValue(jsonValue)
        } yield fn(a, b)
      )
    }

    private[Decode] case class Map3[A, B, C, V](
        fn: (A, B, C) => V,
        decoderA: Decoder[A],
        decoderB: Decoder[B],
        decoderC: Decoder[C]
    ) extends Decoder[V] {
      def decodeValue(
          jsonValue: org.morphir.sdk.json.Decode.Value
      ): DecodeResult[V] = (
        for {
          a <- decoderA.decodeValue(jsonValue)
          b <- decoderB.decodeValue(jsonValue)
          c <- decoderC.decodeValue(jsonValue)
        } yield fn(a, b, c)
      )
    }

    private[Decode] case class Map4[A, B, C, D, V](
        fn: (A, B, C, D) => V,
        decoderA: Decoder[A],
        decoderB: Decoder[B],
        decoderC: Decoder[C],
        decoderD: Decoder[D]
    ) extends Decoder[V] {
      def decodeValue(
          jsonValue: org.morphir.sdk.json.Decode.Value
      ): DecodeResult[V] = (
        for {
          a <- decoderA.decodeValue(jsonValue)
          b <- decoderB.decodeValue(jsonValue)
          c <- decoderC.decodeValue(jsonValue)
          d <- decoderD.decodeValue(jsonValue)
        } yield fn(a, b, c, d)
      )
    }

    private[Decode] case class Map5[A, B, C, D, E, V](
        fn: (A, B, C, D, E) => V,
        decoderA: Decoder[A],
        decoderB: Decoder[B],
        decoderC: Decoder[C],
        decoderD: Decoder[D],
        decoderE: Decoder[E]
    ) extends Decoder[V] {
      def decodeValue(
          jsonValue: org.morphir.sdk.json.Decode.Value
      ): DecodeResult[V] = (
        for {
          a <- decoderA.decodeValue(jsonValue)
          b <- decoderB.decodeValue(jsonValue)
          c <- decoderC.decodeValue(jsonValue)
          d <- decoderD.decodeValue(jsonValue)
          e <- decoderE.decodeValue(jsonValue)
        } yield fn(a, b, c, d, e)
      )
    }

    private[Decode] case class Map6[A, B, C, D, E, F, V](
        fn: (A, B, C, D, E, F) => V,
        decoderA: Decoder[A],
        decoderB: Decoder[B],
        decoderC: Decoder[C],
        decoderD: Decoder[D],
        decoderE: Decoder[E],
        decoderF: Decoder[F]
    ) extends Decoder[V] {
      def decodeValue(
          jsonValue: org.morphir.sdk.json.Decode.Value
      ): DecodeResult[V] = (
        for {
          a <- decoderA.decodeValue(jsonValue)
          b <- decoderB.decodeValue(jsonValue)
          c <- decoderC.decodeValue(jsonValue)
          d <- decoderD.decodeValue(jsonValue)
          e <- decoderE.decodeValue(jsonValue)
          f <- decoderF.decodeValue(jsonValue)
        } yield fn(a, b, c, d, e, f)
      )
    }

    private[Decode] case class Map7[A, B, C, D, E, F, G, V](
        fn: (A, B, C, D, E, F, G) => V,
        decoderA: Decoder[A],
        decoderB: Decoder[B],
        decoderC: Decoder[C],
        decoderD: Decoder[D],
        decoderE: Decoder[E],
        decoderF: Decoder[F],
        decoderG: Decoder[G]
    ) extends Decoder[V] {
      def decodeValue(
          jsonValue: org.morphir.sdk.json.Decode.Value
      ): DecodeResult[V] = (
        for {
          a <- decoderA.decodeValue(jsonValue)
          b <- decoderB.decodeValue(jsonValue)
          c <- decoderC.decodeValue(jsonValue)
          d <- decoderD.decodeValue(jsonValue)
          e <- decoderE.decodeValue(jsonValue)
          f <- decoderF.decodeValue(jsonValue)
          g <- decoderG.decodeValue(jsonValue)
        } yield fn(a, b, c, d, e, f, g)
      )
    }

    private[Decode] case class Map8[A, B, C, D, E, F, G, H, V](
        fn: (A, B, C, D, E, F, G, H) => V,
        decoderA: Decoder[A],
        decoderB: Decoder[B],
        decoderC: Decoder[C],
        decoderD: Decoder[D],
        decoderE: Decoder[E],
        decoderF: Decoder[F],
        decoderG: Decoder[G],
        decoderH: Decoder[H]
    ) extends Decoder[V] {
      def decodeValue(
          jsonValue: org.morphir.sdk.json.Decode.Value
      ): DecodeResult[V] = (
        for {
          a <- decoderA.decodeValue(jsonValue)
          b <- decoderB.decodeValue(jsonValue)
          c <- decoderC.decodeValue(jsonValue)
          d <- decoderD.decodeValue(jsonValue)
          e <- decoderE.decodeValue(jsonValue)
          f <- decoderF.decodeValue(jsonValue)
          g <- decoderG.decodeValue(jsonValue)
          h <- decoderH.decodeValue(jsonValue)
        } yield fn(a, b, c, d, e, f, g, h)
      )
    }

  }

  sealed abstract class Error {
    def asDecodeResult[A]: DecodeResult[A] =
      Err(this)
  }
  object Error {
    case class Field(name: String, error: Error) extends Error
    case class Index(index: Int, error: Error) extends Error
    case class OneOf(errors: List[Error]) extends Error {
      override def toString() = errors.mkString(",")
    }
    case class Failure(message: String, json: Value) extends Error

    def asErrorResult[A](error: Error): DecodeResult[A] =
      Err(error)

    def oneOf(firstError: Error, otherErrors: Error*): Error =
      OneOf(List(firstError) ++ otherErrors)

    def oneOf(errors: List[Error]): Error =
      OneOf(errors)
  }

  def indent(str: String): String =
    str.split("\n").mkString("\n    ")

}
