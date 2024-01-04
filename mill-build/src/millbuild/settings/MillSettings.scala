package millbuild.settings

import zio.config.magnolia.deriveConfig
import zio.Config

case class MillSettings(
  scalaVersion: String = MillSettings.defaultScalaVersion
)

object MillSettings {
  val config: Config[MillSettings]                          = deriveConfig[MillSettings]
  lazy val default: MillSettings                            = MillSettings()
  lazy val defaultScalaVersion                              = "2.13.10"
  implicit val rw: upickle.default.ReadWriter[MillSettings] = upickle.default.macroRW
}
