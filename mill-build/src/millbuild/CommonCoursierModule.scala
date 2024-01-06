package millbuild

import mill._, scalalib._
import coursier.MavenRepository

trait CommonCoursierModule extends CoursierModule {
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

  def repositoriesTask = T.task {
    super.repositoriesTask() ++ Seq(
      MavenRepository("https://oss.sonatype.org/content/repositories/releases"),
      MavenRepository("https://oss.sonatype.org/content/repositories/snapshots"),
      MavenRepository("http://dl.bintray.com/spark-packages/maven")
    )
  }

  val forcedVersions = Seq(
    ("org.apache.ant", "ant", "1.10.12"),
    ("commons-io", "commons-io", "2.11.0"),
    ("com.google.code.gson", "gson", "2.9.0"),
    ("com.google.protobuf", "protobuf-java", "3.21.2"),
    ("com.google.guava", "guava", "31.1-jre"),
    ("org.jsoup", "jsoup", "1.15.3")
  )
}
