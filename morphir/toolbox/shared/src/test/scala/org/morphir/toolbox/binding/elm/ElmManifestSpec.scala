package org.morphir.toolbox.binding.elm

import java.nio.file.Paths

import io.circe.Json
import io.circe.syntax._
import zio.test._
import zio.test.Assertion._

object ElmManifestSpec extends DefaultRunnableSpec {
  def spec = suite("ElmManifest Spec")(
    suite("Serializing to JSON")(
      test("An ElmManifest should serialize to JSON") {
        val manifest = ElmManifest("hello-world", Paths.get("./src/elm"), List("Hello"))
        val json     = manifest.asJson
        assert(json)(
          equalTo(
            Json.obj(
              "name"            -> Json.fromString("hello-world"),
              "sourceDirectory" -> Json.fromString("./src/elm"),
              "exposedModules"  -> Json.arr(Json.fromString("Hello"))
            )
          )
        )
      }
    )
  )
}
