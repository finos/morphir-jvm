package org.morphir.toolbox.core.codecs

import toml.Codec.Defaults
import toml._

class TomlCodec[A](val self: Codec[A]) extends AnyVal {
  def map[B](f: A => B): Codec[B] = TomlCodec.instance {
    case (value, defaults, index) =>
      self(value, defaults, index) match {
        case Right(value) => Right(f(value))
        case left         => left.asInstanceOf[Left[Parse.Error, B]]
      }
  }

  def mapResult[B](f: A => Either[Parse.Error, B]): Codec[B] =
    TomlCodec.instance {
      case (value, defaults, index) =>
        self(value, defaults, index) match {
          case Right(value) => f(value)
          case left         => left.asInstanceOf[Left[Parse.Error, B]]
        }
    }
}

object TomlCodec {
  def instance[A](
      f: (Value, Defaults, Int) => Either[Parse.Error, A]
  ): Codec[A] =
    new Codec[A] {
      override def apply(
          value: Value,
          defaults: Defaults,
          index: Int
      ): Either[Parse.Error, A] =
        f(value, defaults, index)
    }
}
