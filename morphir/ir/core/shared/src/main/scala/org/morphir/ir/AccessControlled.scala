package org.morphir.ir

import upickle.default.{readwriter, ReadWriter => RW}
import upickle.default._

import upickle.core.Abort
import ujson.Value.InvalidData

object AccessControlled {

  sealed abstract class AccessControlled[A] extends Product with Serializable {

    def value: A

    def encodeToJson(implicit encoder: Writer[A]): ujson.Value

    def withPublicAccess: Option[A] = this match {
      case AccessControlled.Public(v) => Some(v)
      case _                          => None
    }

    def withPrivateAccess: A = this match {
      case AccessControlled.Public(v)  => v
      case AccessControlled.Private(v) => v
    }

    def map[B](fn: A => B): AccessControlled[B] =
      this match {
        case Public(value)  => Public(fn(value))
        case Private(value) => Private(fn(value))
      }
  }

  case class Public[A] private[ir] (value: A) extends AccessControlled[A] {

    def encodeToJson(implicit encoder: Writer[A]): ujson.Value =
      ujson.Obj(
        "$type" -> ujson.Str("public"),
        "value" -> writeJs(value)
      )
  }
  object Public {
    implicit def readWriter[A: ReadWriter]: RW[Public[A]] =
      readwriter[ujson.Value].bimap[Public[A]](
        ac => ac.encodeToJson,
        (json: ujson.Value) => {
          json("$type") match {
            case ujson.Str("public") =>
              val value =
                read[A](json("value"))
              Public(value)
            case _ => throw new Abort("expected the tag to be public")
          }
        }
      )
  }

  case class Private[A] private[AccessControlled] (value: A)
      extends AccessControlled[A] {
    def encodeToJson(implicit encoder: Writer[A]): ujson.Value =
      ujson.Obj(
        "$type" -> ujson.Str("private"),
        "value" -> writeJs(value)
      )
  }

  object Private {
    implicit def readWriter[A: ReadWriter]: RW[Private[A]] =
      readwriter[ujson.Value].bimap[Private[A]](
        ac => ac.encodeToJson,
        (json: ujson.Value) => {
          val value = read[A](json("value"))
          Private(value)
        }
      )
  }

  implicit def readWriter[A: ReadWriter]: RW[AccessControlled[A]] =
    readwriter[ujson.Value].bimap[AccessControlled[A]](
      encodeAccessControlled[A],
      decodeAccessControlled
    )

  def publicAccess[A](value: A): AccessControlled[A] =
    Public(value)

  def privateAccess[A](value: A): AccessControlled[A] =
    Private(value)

  def `public`[A](value: A): AccessControlled[A] =
    Public(value)

  def `private`[A](value: A): AccessControlled[A] =
    Private(value)

  @inline def withPublicAccess[A](ac: AccessControlled[A]): Option[A] =
    ac.withPublicAccess

  @inline def withPrivateAccess[A](ac: AccessControlled[A]): A =
    ac.withPrivateAccess

  def encodeAccessControlled[A: Writer](
      ac: AccessControlled[A]
  ): ujson.Value = ac.encodeToJson

  def decodeAccessControlled[A: Reader](
      json: ujson.Value
  ): AccessControlled[A] = {
    json("$type") match {
      case ujson.Str("public")  => Public(read[A](json("value")))
      case ujson.Str("private") => Private(read[A](json("value")))
      case _ =>
        throw new InvalidData(
          json,
          "The provided JSON is missing a valid $type tag attribute."
        )
    }
  }
}
