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
          actual == Some("John Doe")
        )
      }
    )
  )
}
