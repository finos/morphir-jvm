package millbuild.crossplatform
import mill._
import mill.scalalib._
import _root_.millbuild.CommonCrossScalaModule

trait CrossPlatformScalaModule extends PlatformScalaModule with CrossScalaModule with CommonCrossScalaModule {
  def crossPlatformSourceSuffixes(srcFolderName: String) =
    for {
      versionSuffix  <- Seq("") ++ scalaVersionDirectoryNames
      platformSuffix <- Seq("") ++ platform.suffixes
    } yield (versionSuffix, platformSuffix) match {
      case ("", "")     => srcFolderName
      case ("", suffix) => s"$srcFolderName-$suffix"
      case (suffix, "") => s"$srcFolderName-$suffix"
      case (vs, ps)     => s"$srcFolderName-$vs-$ps"
    }

  def crossPlatformRelativeSourcePaths(srcFolderName: String): Seq[os.RelPath] =
    for {
      versionSuffix  <- Seq("") ++ scalaVersionDirectoryNames
      platformSuffix <- Seq("") ++ platform.suffixes
    } yield (versionSuffix, platformSuffix) match {
      case ("", "")     => os.rel / srcFolderName
      case ("", suffix) => os.rel / suffix / srcFolderName
      case (suffix, "") => os.rel / s"$srcFolderName-$suffix"
      case (vs, ps)     => os.rel / ps / s"$srcFolderName-$vs"
    }

  def crossPlatformSources: T[Seq[PathRef]] = T.sources {
    platformFolderMode() match {
      case Platform.FolderMode.UseSuffix =>
        crossPlatformSourceSuffixes("src").map(suffix => PathRef(millSourcePath / suffix))
      case Platform.FolderMode.UseNesting =>
        crossPlatformRelativeSourcePaths("src").map(subPath => PathRef(millSourcePath / subPath))
      case Platform.FolderMode.UseBoth =>
        (crossPlatformSourceSuffixes("src").map(suffix => PathRef(millSourcePath / suffix)) ++
          crossPlatformRelativeSourcePaths("src").map(subPath => PathRef(millSourcePath / subPath))).distinct
    }
  }

  def platformSpecificModuleDeps: Seq[CrossPlatform]         = Seq.empty
  def platformSpecificCompiledModuleDeps: Seq[CrossPlatform] = Seq.empty

  override def moduleDeps = super.moduleDeps ++ platformSpecificModuleDeps.flatMap { case p =>
    p.childPlatformModules(platform)
  }
  override def compileModuleDeps =
    super.compileModuleDeps ++ platformSpecificCompiledModuleDeps.flatMap(_.childPlatformModules(platform))

  //     container.moduleDeps.map(innerModule).asInstanceOf[Seq[this.type]]
  // override def compileModuleDeps = super.compileModuleDeps ++
  //     container.compileModuleDeps.map(innerModule).asInstanceOf[Seq[this.type]]

  // def innerModule(module:CrossPlatformModule) = module match {}

  def platformFolderMode: T[Platform.FolderMode] = T(Platform.FolderMode.UseNesting)
  def platform: Platform
  def knownPlatforms: T[Seq[Platform]] = T(Platform.all.toSeq)

  def sources = T.sources {
    crossPlatformSources()
  }
}
