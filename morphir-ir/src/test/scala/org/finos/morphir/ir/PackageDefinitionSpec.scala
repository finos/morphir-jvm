package org.finos.morphir.ir

import zio.test.{ DefaultRunnableSpec, assert }
import zio.test.Assertion.*

import scala.collection.immutable.ListMap

object PackageDefinitionSpec extends DefaultRunnableSpec {
  def spec = suite("PackageDefinition spec")(
    test("It should be possible to create an empty PackageDefinition") {
      val actual = PackageDefinition.empty
      assert(actual)(equalTo(PackageDefinition[Unit, Unit](ListMap.empty)))
    }
  )
}
