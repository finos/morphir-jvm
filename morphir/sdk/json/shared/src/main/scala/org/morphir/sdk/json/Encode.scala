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
import io.circe._

object Encode {
  type Value = io.circe.Json

  def encode(indent: Int): Value => String =
    (json: Value) => {
      val printer = Printer.indented(" ".repeat(indent))
      json.printWith(printer)
    }

  def string(value: String): Value = Json.fromString(value)

  def int(value: Int): Value = Json.fromInt(value)

  def long(value: Long): Value = Json.fromLong(value)

  def float(value: Float): Value = Json.fromFloatOrString(value)

  def double(value: Double): Value = Json.fromDoubleOrString(value)

  def bool(value: Boolean): Value = Json.fromBoolean(value)

  val `null`: Value = Json.Null

  val Null: Value = Json.Null

  def list[A](fn: A => Value)(items: List[A]): Value =
    Json.arr(items.map(fn): _*)

  def array[A](fn: A => Value)(items: Array[A]): Value =
    Json.arr(items.map(fn): _*)

  def set[A](fn: A => Value)(items: Set[A]): Value =
    Json.arr(items.map(fn).toIndexedSeq: _*)

  def `object`(entries: List[(String, Value)]): Value =
    entries match {
      case Nil   => Json.obj()
      case items => Json.obj(items: _*)
    }

  def obj(entries: List[(String, Value)]): Value = `object`(entries)

  def dict[K, V](
      keyMap: K => String
  )(valueMap: V => Value)(fields: Map[K, V]): Value =
    Json.fromFields(
      fields.map { case (key, value) => keyMap(key) -> valueMap(value) }
    )

}
