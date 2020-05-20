package org.morphir.workspace.config

import org.morphir.workspace.config.project.ProjectSet
import zio.config.read
import zio.config.typesafe.TypesafeConfigSource
import zio.test._
import zio.test.Assertion._

object ProjectSetSpec extends DefaultRunnableSpec {
  def spec = suite("ProjectSet Spec")(
    suite("Reading from HOCON")(
      test("Reading from HOCON with multiple projects") {
        val hocon =
          """
            |projectA {
            | sourceDirectory ="src/elm"
            |} 
            |
            |projectB {
            | sourceDirectory = "src"
            |}
            |""".stripMargin

        val parsedSource = TypesafeConfigSource.fromHoconString(hocon)
        val result       = parsedSource.flatMap(source => read(ProjectSet.configDescriptor from source))
        assert(result)(isRight(anything))
      }
    )
  )
}
