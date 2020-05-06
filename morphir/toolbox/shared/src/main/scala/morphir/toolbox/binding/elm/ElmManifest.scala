package morphir.toolbox.binding.elm

import io.circe.{ Decoder, Encoder }
import java.nio.file.{ Path, Paths }

case class ElmManifest(name: String, sourceDirectory: Path, exposedModules: List[String]) {}

object ElmManifest {
  implicit val elmManifestEncoder: Encoder[ElmManifest] =
    Encoder.forProduct3("name", "sourceDirectory", "exposedModules")(manifest =>
      (manifest.name, manifest.sourceDirectory.toString, manifest.exposedModules)
    )

  implicit val elmManifestDecoder: Decoder[ElmManifest] =
    Decoder.forProduct3[ElmManifest, String, String, List[String]]("name", "sourceDirectory", "exposedModules") {
      case (name, sourceDirectory, exposedModules) =>
        ElmManifest(name, Paths.get(sourceDirectory), exposedModules)
    }
}
