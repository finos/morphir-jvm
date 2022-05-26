package org.finos.morphir.knowledge.core
import zio.test._

object FieldSpec extends DefaultRunnableSpec {
  def spec = suite("FieldSpec")(
    suite("define") {
      test("define is able to get the name from the variable it is being defined on") {
        val snoop   = Field.define[String]
        val marshal = Field.define[Int]
        val dre     = Field.define[Double]
        assertTrue(snoop.name == "snoop", marshal.name == "marshal", dre.name == "dre")
      }
    }
  )
}
