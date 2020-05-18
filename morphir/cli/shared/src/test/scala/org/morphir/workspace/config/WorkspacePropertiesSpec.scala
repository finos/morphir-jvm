package org.morphir.workspace.config

import zio.test._
import zio.test.Assertion._
import zio.config._
import zio.config.typesafe.TypesafeConfigSource

object WorkspacePropertiesSpec extends DefaultRunnableSpec {
  def spec = suite("WorkspaceProperties Spec")(
    suite("HOCON config reading")(
      test("Should read from HOCON") {
        val hocon =
          """
            |projects {
            | projectA {
            |   sourceDirectory = "src/elm"
            | }
            | 
            | projectB {
            |   sourceDirectory = "src"
            | }
            |}""".stripMargin

        val parsedSource = TypesafeConfigSource.fromHoconString(hocon)
        val result       = parsedSource.flatMap(source => read(WorkspaceProperties.config from source))
        assert(result)(isRight(anything))
      }
    )
  )
}
