import sbt._

object Dependencies {
  case object dev {
    case object zio {
      val zio = "dev.zio" %% "zio" % "1.0.9"
    }
  }
}
