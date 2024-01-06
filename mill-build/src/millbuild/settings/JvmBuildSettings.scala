package millbuild.settings

import zio.ConfigProvider
import zio.config.magnolia.deriveConfig

final case class JvmBuildSettings(enable: Boolean = true)

object JvmBuildSettings {
  val config                         = deriveConfig[JvmBuildSettings]
  lazy val default: JvmBuildSettings = JvmBuildSettings()

  implicit lazy val rw: upickle.default.ReadWriter[JvmBuildSettings] = upickle.default.macroRW
}
