package build 
import mill._, scalalib._

object `package` extends MillBuildRootModule {
  override def scalacOptions = super.scalacOptions() ++ Seq("-feature", "-deprecation", "-unchecked")
  override def ivyDeps = super.ivyDeps() ++ Seq(
    ivy"dev.zio::zio:2.0.21",
    ivy"dev.zio::zio-config:4.0.0",
    ivy"dev.zio::zio-config-magnolia:4.0.0",
    ivy"dev.zio::zio-config-typesafe:4.0.0",
    ivy"dev.zio::zio-config-yaml:4.0.0",
    ivy"dev.zio::zio-config-refined:4.0.0",
    ivy"com.carlosedp::mill-aliases::0.4.1",
  )
}
