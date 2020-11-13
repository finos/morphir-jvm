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

import morphir.ir.AccessControlled.{ Private, Public }
import morphir.ir.codec.accessControlledCodecs

sealed abstract class AccessControlled[+A] extends Product with Serializable {

  def value: A

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

  def flatMap[B](fn: A => AccessControlled[B]): AccessControlled[B] = this match {
    case Public(value)  => fn(value)
    case Private(value) => fn(value)
  }

  def fold[B](whenPublic: A => B, whenPrivate: A => B): B = this match {
    case Public(value)  => whenPublic(value)
    case Private(value) => whenPrivate(value)
  }
}

object AccessControlled extends accessControlledCodecs.AccessControlledCodec {

  sealed abstract class Access extends Serializable
  object Access {
    case object Public  extends Access
    case object Private extends Access
  }

  final case class Public[+A] private[AccessControlled] (value: A) extends AccessControlled[A]

  final case class Private[+A] private[AccessControlled] (value: A) extends AccessControlled[A] {}

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
}
