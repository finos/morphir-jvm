package morphir.toolbox.workspace.config

import morphir.toolbox.core.ProjectPath
import zio.test.Assertion.{ equalTo, isRight }
import zio.test.{ assert, suite, test, DefaultRunnableSpec }

object WorkspaceSettingsSpec extends DefaultRunnableSpec {
  def spec = suite("WorkspaceConfig Spec")(
    suite("Parsing from TOML")(
      test("Should parse from TOML") {
        val doc =
          """
            |[projects.foo]
            |[projects.bar]
            |projectDir = "models/bar/"
            |""".stripMargin

        val config = WorkspaceSettings.fromToml(doc)
        assert(config)(
          isRight(
            equalTo(
              WorkspaceSettings(
                Map(
                  "foo" -> ProjectSettings(),
                  "bar" -> ProjectSettings(projectDir = Some(ProjectPath.of("models/bar/")))
                )
              )
            )
          )
        )
      }
    )
  )
}
