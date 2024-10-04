package build 
import mill._, scalalib._

object `package` extends MillBuildRootModule {
  override def scalacOptions = super.scalacOptions() ++ Seq("-feature", "-deprecation", "-unchecked")
  override def ivyDeps = super.ivyDeps() ++ Seq(
    ivy"dev.zio::zio:2.1.9",
    ivy"dev.zio::zio-config:4.0.2",
    ivy"dev.zio::zio-config-magnolia:4.0.2",
    ivy"dev.zio::zio-config-typesafe:4.0.2",
    ivy"dev.zio::zio-config-yaml:4.0.2",
    ivy"dev.zio::zio-config-refined:4.0.2",
    ivy"com.carlosedp::mill-aliases::0.4.1",
    ivy"io.chris-kipp::mill-ci-release::0.1.10",
    ivy"com.goyeau::mill-scalafix::0.3.1",
    ivy"com.google.jimfs:jimfs:1.3.0",
    ivy"io.github.davidgregory084::mill-tpolecat::0.3.5",
    ivy"org.yaml:snakeyaml:1.33",
  )
}
