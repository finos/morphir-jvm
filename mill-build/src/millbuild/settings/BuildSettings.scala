package millbuild.settings
import zio.{ ConfigProvider, Unsafe, Runtime }
import zio.config._
import zio.config.magnolia.deriveConfig
import zio.config.typesafe._
import zio.config.yaml._
import com.typesafe.config.ConfigFactory
import zio.Config

final case class BuildSettings(
  jvm: JvmBuildSettings = JvmBuildSettings(),
  js: ScalaJsBuildSettings = ScalaJsBuildSettings(),
  native: ScalaNativeBuildSettings = ScalaNativeBuildSettings(),
  mill: MillSettings = MillSettings(),
  scala: ScalaSettings = ScalaSettings()
)

object BuildSettings {

  val config: Config[BuildSettings] = deriveConfig[BuildSettings]
  lazy val default: BuildSettings   = BuildSettings()

  implicit lazy val rw: upickle.default.ReadWriter[BuildSettings] = upickle.default.macroRW

  lazy val buildUserHoconFileConfigProvider: ConfigProvider =
    ConfigProvider.fromHoconFile((os.pwd / "build.user.conf").toIO)

  lazy val buildEnvConfigProvider: ConfigProvider =
    ConfigProvider.envProvider.nested("morphir_build")

  lazy val propertiesFileConfigProvider: ConfigProvider =
    ConfigProvider.propsProvider.nested("morphir.build")

  // .fromPropertiesFile((os.pwd / "build.user.properties").toIO)

  lazy val buildUserYamlFileConfigProvider =
    ConfigProvider.fromYamlPath((os.pwd / "build.user.yaml").wrapped)

  def load(): BuildSettings = Unsafe.unsafe { implicit u =>
    Runtime.default.unsafe
      .run(
        loadSettings()
      )
      .getOrThrowFiberFailure()
  }

  def loadSettings() =
    read(
      BuildSettings.config from (
        buildEnvConfigProvider orElse
          propertiesFileConfigProvider orElse
          buildUserHoconFileConfigProvider // orElse
          // buildUserYamlFileConfigProvider
      )
    )

}
