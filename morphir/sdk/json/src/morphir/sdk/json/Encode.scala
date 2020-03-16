package morphir.sdk.json

import upickle.default.{readwriter, ReadWriter => RW, macroRW}
import upickle.default._

object Encode {
  type Value = ujson.Value

  def encode(indent: Int): Value => String =
    (json: Value) => write(json, indent)

  def string(value: String): Value = ujson.Str(value)

  def int(value: Int): Value = ujson.Num(value)

  def long(value: Long): Value = ujson.Num(value)

  def float(value: Float): Value = ujson.Num(value)

  def double(value: Double): Value = ujson.Num(value)

  def bool(value: Boolean): Value = ujson.Bool(value)

  val `null`: Value = ujson.Null

  val Null: Value = ujson.Null

  def list[A](fn: A => Value): List[A] => Value =
    (items: List[A]) => ujson.Arr(items.map(fn))

  def list[A](fn: A => Value, items: List[A]): Value =
    ujson.Arr(items.map(fn))

  def array[A](fn: A => Value): Array[A] => Value =
    (items: Array[A]) => ujson.Arr(items.map(fn).toIndexedSeq)

  def array[A](fn: A => Value, items: Array[A]): Value =
    ujson.Arr(items.map(fn).toIndexedSeq)

  def set[A](fn: A => Value): Set[A] => Value =
    (items: Set[A]) => ujson.Arr(items.map(fn))

  def set[A](fn: A => Value, items: Set[A]): Value =
    ujson.Arr(items.map(fn))

  def `object`(entries: List[(String, Value)]): Value =
    entries match {
      case Nil          => ujson.Obj()
      case head :: Nil  => ujson.Obj(head)
      case head :: rest => ujson.Obj(head, rest: _*)
    }

  def obj(entries: List[(String, Value)]) = `object`(entries)

  def dict[K, V](keyMap: K => String) =
    (valueMap: V => Value) =>
      (props: Map[K, V]) =>
        ujson.Obj.from(
          props
            .map { case (key, value) => keyMap(key) -> valueMap(value) }
        )

}
