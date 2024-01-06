package millbuild.crossplatform

object DevMode {
  val devMode: Boolean = System.getenv("MORPHIR_SCALA_DEV_MODE") == "true"
}
