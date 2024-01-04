import mill._
import mill.scalalib._
import mill.util.Jvm
import mill.define.Target
import os.Path

trait MDocModule extends ScalaModule {

  def scalaMdocVersion: T[String] = T("2.4.0")

  def scalaMdocDep: T[Dep] = T(ivy"org.scalameta::mdoc:${scalaMdocVersion()}")

  def watchedMDocsDestination: T[Option[Path]] = T(None)

  override def ivyDeps = T {
    super.ivyDeps() ++ Agg(scalaMdocDep())
  }

  // where do the mdoc sources live ?
  def mdocSources = T.sources(super.millSourcePath)

  def mdoc: T[PathRef] = T {

    val cp = runClasspath().map(_.path)

    val dir = T.dest.toIO.getAbsolutePath
    val dirParams =
      mdocSources().map(pr => Seq(s"--in", pr.path.toIO.getAbsolutePath, "--out", dir)).iterator.flatten.toSeq

    Jvm.runLocal("mdoc.Main", cp, dirParams)

    PathRef(T.dest)
  }

  def mdocWatch() = T.command {

    watchedMDocsDestination() match {
      case None =>
        throw new Exception("watchedMDocsDestination is not set, so we dant know where to put compiled md files")
      case Some(p) =>
        val cp = runClasspath().map(_.path)
        val dirParams = mdocSources()
          .map(pr => Seq(s"--in", pr.path.toIO.getAbsolutePath, "--out", p.toIO.getAbsolutePath))
          .iterator
          .flatten
          .toSeq
        Jvm.runLocal("mdoc.Main", cp, dirParams ++ Seq("--watch"))
    }

  }
}

trait Docusaurus2Module extends Module {

  def docusaurusSources: Sources
  def compiledMdocs: Sources

  def yarnInstall: T[PathRef] = T {
    val baseDir = T.dest

    docusaurusSources().foreach { pr =>
      os.list(pr.path)
        .foreach(p =>
          os.copy.into(
            p,
            baseDir,
            followLinks = true,
            replaceExisting = true,
            copyAttributes = true,
            createFolders = true,
            mergeFolders = false
          )
        )
    }

    val process = Jvm.spawnSubprocess(
      commandArgs = Seq(
        "yarn",
        "install",
        "--check-cache"
      ),
      envArgs = Map.empty,
      workingDir = T.dest
    )
    process.join()
    T.log.info(new String(process.stdout.bytes))
    PathRef(T.dest)
  }

  def installedDocusaurusSources = T.source(yarnInstall().path)

  def docusaurusBuild: T[PathRef] = T {
    val workDir                = T.dest
    val docusaurusInstallation = installedDocusaurusSources()
    val yarnSetup              = docusaurusInstallation.path

    os.copy(
      yarnSetup,
      workDir,
      followLinks = true,
      replaceExisting = true,
      copyAttributes = true,
      createFolders = true,
      mergeFolders = false
    )

    val docsDir = workDir / "docs"
    os.makeDir.all(docsDir)
    os.list(docsDir).foreach(os.remove.all)

    Seq(docusaurusInstallation).foreach { pr =>
      val bd = pr.path
      os.walk(pr.path / "docs").foreach { p =>
        val relPath = p.relativeTo(bd / "docs")
        T.log.info(relPath.toString())
        if (p.toIO.isFile) {
          val target = docsDir / relPath
          os.makeDir.all(target)
          os.copy.over(p, docsDir / relPath)
        }
      }
    }

    compiledMdocs().foreach { pr =>
      os.list(pr.path)
        .foreach(p =>
          os.copy.into(
            p,
            docsDir,
            followLinks = true,
            replaceExisting = true,
            copyAttributes = true,
            createFolders = true,
            mergeFolders = true
          )
        )
    }

    // For some reason we cant run yarn build otherwise
    // val p1 = Jvm.spawnSubprocess(
    //   commandArgs = Seq(
    //     "yarn",
    //     "install",
    //     "--force"
    //   ),
    //   envArgs = Map.empty,
    //   workingDir = workDir
    // )

    // p1.join()

    val p2 = Jvm.spawnSubprocess(
      commandArgs = Seq(
        "yarn",
        "build"
      ),
      envArgs = Map.empty,
      workingDir = workDir
    )

    p2.join()

    PathRef(workDir)
  }

  def docusaurusServe() = T.command {
    val workDir = docusaurusBuild().path
    Jvm.runSubprocess(
      commandArgs = Seq(
        "yarn",
        "start"
      ),
      envArgs = T.env,
      workingDir = workDir
    )
  }
}
