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
