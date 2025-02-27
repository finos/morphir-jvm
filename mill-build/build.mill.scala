package build
import mill._, scalalib._

object `package` extends MillBuildRootModule {
  override def scalacOptions = super.scalacOptions() ++ Seq("-feature", "-deprecation", "-unchecked")
  override def ivyDeps = super.ivyDeps() ++ Seq(
    ivy"dev.zio::zio:2.1.16",
    ivy"dev.zio::zio-config:4.0.2",
    ivy"dev.zio::zio-config-magnolia:4.0.2",
    ivy"dev.zio::zio-config-typesafe:4.0.2",
    ivy"dev.zio::zio-config-yaml:4.0.2",
    ivy"dev.zio::zio-config-refined:4.0.2",
    ivy"com.carlosedp::mill-aliases::0.4.1",
    ivy"com.goyeau::mill-scalafix::0.3.1",
    ivy"com.google.jimfs:jimfs:1.3.0",
    ivy"io.github.davidgregory084::mill-tpolecat::0.3.5",
    ivy"de.tototec::de.tobiasroeser.mill.vcs.version::0.4.0"
  )

  override def mapDependencies: Task[coursier.Dependency => coursier.Dependency] = T.task {
    super.mapDependencies().andThen { dep =>
      forcedVersions
        .find(t => t._1 == dep.module.organization.value && t._2 == dep.module.name.value)
        .map { forced =>
          val newDep = dep.withVersion(forced._3)
          T.log.debug(s"Mapping $dep to $newDep")
          newDep
        }
        .getOrElse(dep)
    }
  }

  val forcedVersions = Seq(
    ("com.google.guava", "guava", "32.0.1-jre"),
    ("org.yaml", "snakeyaml", "1.33")
  )
}
