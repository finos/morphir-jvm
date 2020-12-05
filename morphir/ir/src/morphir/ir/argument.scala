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

import morphir.ir.codec.argumentCodecs

object argument {

  def apply[A](name: Name, value: A): Argument[A] = Argument(name, value)

  final case class Argument[+A](name: Name, value: A) {

    def map[B](f: (Name, A) => (Name, B)): Argument[B] = {
      val (newName, newValue) = f(name, value)
      Argument(newName, newValue)
    }

    def mapValue[B](f: A => B): Argument[B] = Argument(name, f(value))
  }

  object Argument extends argumentCodecs.ArgumentCodec {
    def fromTuple[A](tuple: (Name, A)): Argument[A] = Argument(tuple._1, tuple._2)
  }

  type ArgumentList[+A] = List[Argument[A]]

  implicit class ArgumentListOps[A](private val self: ArgumentList[A]) extends AnyVal {
    def mapValue[B](f: A => B): ArgumentList[B] = self.map(arg => arg.mapValue(f))
  }
}
