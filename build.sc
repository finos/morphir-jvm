import $ivy.`com.lihaoyi::mill-contrib-bloop:$MILL_VERSION`
import $ivy.`com.lihaoyi::mill-contrib-scalapblib:$MILL_VERSION`
import $ivy.`com.github.carueda::jbuildinfo:0.1.2`

import mill._
import mill.scalalib._
import publish._
import mill.scalalib.scalafmt._
import com.github.carueda.mill.JBuildInfo
import contrib.scalapblib._
import coursier.maven.MavenRepository

val productVersion = "0.1.0"

object morphir extends Module {
  object core extends Cross[CoreModule]("2.12.10", "2.13.1") {}

  object cli extends Cross[CliModule]("2.12.10", "2.13.1") {}

  object daemon extends Cross[DaemonModule]("2.12.10", "2.13.1") {}

}

class CoreModule(val crossScalaVersion: String)
    extends CrossScalaModule
    with MorphirCommonModule
    with PublishModule {

  def publishVersion = productVersion
  def ivyDeps = Agg(
    ivy"com.lihaoyi::upickle:1.0.0"
  )

  def pomSettings = PomSettings(
    description = "Morphir core package",
    organization = "morphir",
    url = "https://github.com/MorganStanley/morphir-jvm",
    licenses = Seq(License.`Apache-2.0`),
    versionControl = VersionControl.github("MorganStanley", "morphir-jvm"),
    developers = Seq(
      Developer(
        "DamianReeves",
        "Damian Reeves",
        "https://github.com/DamianReeves"
      )
    )
  )

  object test extends Tests with MorphirTestModule {}
}

class CliModule(val crossScalaVersion: String)
    extends CrossScalaModule
    with MorphirCommonModule {
  def ivyDeps = Agg(
    ivy"dev.zio::zio-config:${Versions.`zio-config`}"
  )
  object test extends Tests with MorphirTestModule {}
}

class DaemonModule(val crossScalaVersion: String)
    extends CrossScalaModule
    with MorphirCommonModule
    with ScalaPBModule {

  def repositories = super.repositories ++ Seq(
    MavenRepository("https://oss.sonatype.org/content/repositories/snapshots")
  )
  override def ivyDeps = T {
    super.ivyDeps() ++
      Agg(
        ivy"com.thesamet.scalapb::scalapb-runtime:${Versions.scalaPB}"
          .withConfiguration("protobuf"),
        ivy"com.thesamet.scalapb.zio-grpc::zio-grpc-codegen:${Versions.zioGrpcVersion}"
      )
  }

  def scalaPBIncludePath = Seq(scalaPBUnpackProto())

  def scalaPBVersion = Versions.scalaPB
  def scalaPBGrpc = true
}

trait MorphirCommonModule extends ScalafmtModule with ScalaModule {}

trait MorphirBuildInfoModule extends JBuildInfo {
  def buildInfoMembers: T[Map[String, String]] = T {
    Map(
      "name" -> "some name",
      "version" -> productVersion
    )
  }
}

trait MorphirTestModule extends TestModule {
  def ivyDeps = Agg(
    ivy"org.scalatest::scalatest:3.1.1",
    ivy"dev.zio::zio-test:${Versions.zio}",
    ivy"dev.zio::zio-test-sbt:${Versions.zio}"
  )
  def testFrameworks =
    Seq("org.scalatest.tools.Framework", "zio.test.sbt.ZTestFramework")
}

object Versions {
  val zio = "1.0.0-RC18-1"
  val `zio-config` = "1.0.0-RC12"
  val scalaPB = "0.10.0"
  val zioGrpcVersion = "0.0.0+51-4359117f-SNAPSHOT"
}
