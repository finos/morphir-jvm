package millbuild.settings

import zio.config.magnolia.deriveConfig
import zio.Config
import millbuild.crossplatform.DevMode

final case class ScalaSettings(
  defaultVersion: String = ScalaSettings.defaultVersion,
  scala212Version: String = ScalaSettings.defaultScala212Version,
  scala213Version: String = ScalaSettings.defaultScala213Version,
  scala3xVersion: String = ScalaSettings.defaultScala3xVersion,
  crossScalaVersions: List[String] = ScalaSettings.defaultCrossScalaVersions
)

object ScalaSettings {
  import DevMode._

  val config: Config[ScalaSettings]                          = deriveConfig[ScalaSettings]
  lazy val default: ScalaSettings                            = ScalaSettings()
  lazy val defaultVersion                                    = defaultScala3xVersion
  implicit val rw: upickle.default.ReadWriter[ScalaSettings] = upickle.default.macroRW

  val defaultScala212Version = "2.12.20"
  val defaultScala213Version = "2.13.15"
  val defaultScala3xVersion  = "3.3.4"
  val defaultCrossScalaVersions: List[String] =
    if (devMode) List(defaultScala3xVersion)
    else List(defaultScala3xVersion, defaultScala213Version, defaultScala212Version)
}
