package org.morphir.workspace.config

import org.morphir.workspace.config.project.{ ProjectProperties, SourceDirectory }
import zio.config._
import zio.config.typesafe.TypesafeConfigSource
import zio.test._
import zio.test.Assertion._

object ProjectPropertiesSpec extends DefaultRunnableSpec {
  def spec = suite("ProjectProperties Spec")(
    suite("Reading from HOCON")(
      test("Reading valid HOCON") {
        val hocon =
          """
            |sourceDirectory: "src/elm"
            |""".stripMargin

        val parsedSource = TypesafeConfigSource.fromHoconString(hocon)
        val result       = parsedSource.flatMap(source => read(ProjectProperties.configDescriptor from source))
        assert(result)(isRight(equalTo(ProjectProperties(SourceDirectory("src/elm")))))
      }
    )
  )
}
