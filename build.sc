import mill._
import mill.scalalib._
import mill.scalalib.scalafmt._
import $ivy.`com.lihaoyi::mill-contrib-bloop:$MILL_VERSION`

object core extends Cross[CoreModule]("2.12.10", "2.13.1") {}

class CoreModule(val crossScalaVersion: String)
    extends CrossScalaModule
    with MorphirModule {
  def ivyDeps = Agg(
    ivy"com.lihaoyi::upickle::1.0.0"
  )
  object test extends Tests with MorphirTestModule {}
}

trait MorphirModule extends ScalafmtModule { this: ScalaModule => }

trait MorphirTestModule extends TestModule with ScalaModule {
  def ivyDeps = Agg(
    ivy"org.scalatest::scalatest:3.1.1",
    ivy"dev.zio::zio-test:${Versions.zio}",
    ivy"dev.zio::zio-test-sbt:${Versions.zio}"
  )
  def testFrameworks =
    Seq("org.scalatest.tools.Framework", "zio.test.sbt.ZTestFramework")
}

object Versions {
  val zio = "1.0.0-RC17"
}
