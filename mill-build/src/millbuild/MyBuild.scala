package millbuild
import mill._
import mill.define.ExternalModule
import millbuild.settings._

object MyBuild extends ExternalModule {

  lazy val cachedBuildSettings = BuildSettings.load()

  def buildSettings = T.input {
    BuildSettings.load()
  }

  def devMode = T.input { T.env.getOrElse("MORPHIR_SCALA_DEV_MODE", false) == "true" }

  def showBuildSettings() = T.command {
    val settings = buildSettings()
    pprint.pprintln(settings)
    settings
  }

  lazy val millDiscover = mill.define.Discover[this.type]
}
