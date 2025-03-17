package millbuild.settings

import zio.config.magnolia.deriveConfig
import zio.Config

final case class ScalaNativeBuildSettings(
  enable: Boolean = true,
  version: String = ScalaNativeBuildSettings.defaultVersion
)

object ScalaNativeBuildSettings {
  val config: Config[ScalaNativeBuildSettings] = deriveConfig[ScalaNativeBuildSettings]
  lazy val default: ScalaNativeBuildSettings   = ScalaNativeBuildSettings()

  lazy val defaultVersion = "0.5.7"

  implicit lazy val rw: upickle.default.ReadWriter[ScalaNativeBuildSettings] = upickle.default.macroRW
}
