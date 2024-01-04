package millbuild.settings

import zio.config.magnolia.deriveConfig

final case class ScalaJsBuildSettings(enable: Boolean = true, version: String = ScalaJsBuildSettings.defaultVersion)

object ScalaJsBuildSettings {
  val config                             = deriveConfig[ScalaJsBuildSettings]
  lazy val default: ScalaJsBuildSettings = ScalaJsBuildSettings()
  lazy val defaultVersion                = "1.14.0"

  implicit lazy val rw: upickle.default.ReadWriter[ScalaJsBuildSettings] = upickle.default.macroRW
}
