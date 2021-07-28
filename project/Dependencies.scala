import sbt._

object Dependencies {
  case object dev {
    case object zio {
      private val zioVersion = "1.0.9"
      val zio = "dev.zio" %% "zio" % zioVersion
      val `zio-test` = "dev.zio" %% "zio-test" % zioVersion
      val `zio-test-sbt` = "dev.zio" %% "zio-test-sbt" % zioVersion
      
    }
  }

  val zioCommonDeps = Seq(
    dev.zio.zio,
    dev.zio.`zio-test` % "test",
    dev.zio.`zio-test-sbt` % "test"
  )
}
