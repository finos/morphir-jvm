package org.finos.morphir.knowledge.core
import zio.test._

object BindingsSpec extends DefaultRunnableSpec {
  def spec = suite("Bindings Spec")(
    suite("valueOf")(
      test("should return the value when the substitution directly contains the value") {
        val nameField = Field.of[String]("name")
        val bindings  = Bindings.having(nameField -> "John Doe")
        val actual    = bindings.valueOf(nameField)
        assertTrue(
          actual == Some("John Doe")
        )
      },
      test("should return the value when the substitution in-directly contains the value") {
        val nameField  = Field.of[String]("name")
        val aliasField = Field.of[String]("alias")
        val bindings   = Bindings.having(nameField -> "John Doe", aliasField -> nameField)
        val actual     = bindings.valueOf(aliasField)
        assertTrue(
          actual == Some("John Doe"),
          bindings.fields == Set(nameField, aliasField)
        )
      },
      test("Should return None if there is no path to the field value") {
        val a        = Field.of[Int]("a")
        val b        = Field.of[Int]("b")
        val c        = Field.of[Int]("c")
        val bindings = Bindings.having(a -> 42, c -> b)
        val actual   = bindings.valueOf(b)
        assertTrue(
          actual == None,
          bindings.fields == Set(a, c)
        )
      },
      test("should return the value only if it matches the field type") {
        val indirect    = Field.of[Int]("indirectTuple")
        val indirect2   = Field.of[Boolean]("indirect")
        val valueHolder = Field.of[Int]("valueHolder")
        val bindings    = Bindings.having(indirect -> valueHolder, indirect2 -> valueHolder, valueHolder -> 42)
        assertTrue(
          bindings.valueOf(indirect) == Some(42),
          bindings.valueOf(indirect2) == None,
          bindings.valueOf(valueHolder) == Some(42)
        )
      }
    ),
    suite("dynamicValueOf")(
      test("Will return the field value irrespective of type") {
        val indirect    = Field.of[Int]("indirectTuple")
        val indirect2   = Field.of[Boolean]("indirect")
        val valueHolder = Field.of[Int]("valueHolder")
        val bindings    = Bindings.having(indirect -> valueHolder, indirect2 -> valueHolder, valueHolder -> 42)
        assertTrue(
          bindings.dynamicValueOf(indirect) == 42,
          Option(bindings.dynamicValueOf(indirect)) == bindings.valueOf(indirect),
          bindings.dynamicValueOf(indirect2) == 42,
          Option(bindings.dynamicValueOf(indirect2)) != bindings.valueOf(indirect2),
          bindings.dynamicValueOf(valueHolder) == 42,
          Option(bindings.dynamicValueOf(valueHolder)) == bindings.valueOf(valueHolder)
        )
      }
    )
  )
}
