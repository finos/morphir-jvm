package org.finos.morphir.knowledge.core
import zio.test._

object StateSpec extends DefaultRunnableSpec {
  def spec = suite("StateSpec")(
    test("addConstraint adds a constraint to an empty state") {
      val field      = Field.define[String]
      val constraint = FieldConstraint.unconstrained
      val sut        = State.empty

      val actual = sut.addConstraint(field, constraint)
      assertTrue(
        actual.hasConstraint(field),
        actual.constraintsOn(field) == List(constraint)
      )
    },
    test("addConstraint adds a constraint when the constraints on the field is non-empty") {
      val field      = Field.define[String]
      val constraint = FieldConstraint.unconstrained
      val sut        = State.fromFieldConstraints(field -> List(constraint))

      val actual = sut.addConstraint(field, constraint)
      assertTrue(
        actual.hasConstraint(field),
        actual.constraintsOn(field) == List(constraint, constraint)
      )
    },
    test("unify should return the same state if the value is the same") {
      val sut    = State.empty
      val actual = sut.unify(42, 42)
      assertTrue(
        actual == Some(sut)
      )
    },
    test("unify should return the same state if given 2 fields with the same value") {
      val timon   = Field.define[String]
      val pumba   = Field.define[String]
      val rating1 = Field.define[BigDecimal]
      val rating2 = Field.define[BigDecimal]
      val sut = State(
        Fields(
          timon   -> "The Lion King",
          pumba   -> "The Lion King",
          rating1 -> BigDecimal(5.0),
          rating2 -> BigDecimal(5.0)
        )
      )
      val actual = sut.unify(timon, pumba)
      assertTrue(
        sut.unify(timon, pumba) == Some(sut),
        sut.unify(rating1, rating2) == Some(sut)
      )
    }
  )

}
