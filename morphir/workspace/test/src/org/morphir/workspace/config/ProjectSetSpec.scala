/*
Copyright 2020 Morgan Stanley

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/


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
