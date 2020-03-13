object module {
  import java.net.URI
  import java.nio.file.attribute.BasicFileAttributes
  import java.nio.file.{
    FileSystems,
    Files,
    Path,
    SimpleFileVisitor,
    StandardCopyOption
  }

  import coursier.MavenRepository
  import coursier.core.Version
  import mill.define.Sources
  import mill.api.PathRef
  import mill.scalalib.Lib.resolveDependencies
  import mill.scalalib._
  import mill.api.Loose
  import contrib.scalapblib._
  import workers._

  trait ZScalaPBModule extends ScalaPBModule {

    def scalaPBZio: T[Boolean] = T { false }

    def scalaPBPluginArtifacts = T { Seq.empty[String] }

    def scalaPBIncludePath = Seq(scalaPBUnpackProto())

    def compileScalaPB: T[PathRef] = T.persistent {
      ZScalaPBWorkerApi.scalaPBWorker
        .compile(
          scalaPBClasspath().map(_.path),
          scalaPBProtocPath(),
          scalaPBSources().map(_.path),
          scalaPBOptions(),
          T.dest,
          scalaPBIncludePath().map(_.path),
          scalaPBZio(),
          scalaPBPluginArtifacts()
        )
    }
  }

}

object workers {
  import java.io.File
  import java.lang.reflect.Method
  import java.net.URLClassLoader

  import mill.api.PathRef

  class ZScalaPBWorker {

    private var scalaPBInstanceCache = Option.empty[(Long, ZScalaPBWorkerApi)]

    private def scalaPB(
        scalaPBClasspath: Agg[os.Path],
        protocPath: Option[String]
    ) = {
      val classloaderSig =
        scalaPBClasspath.map(p => p.toString().hashCode + os.mtime(p)).sum
      scalaPBInstanceCache match {
        case Some((sig, instance)) if sig == classloaderSig => instance
        case _ =>
          val cl = new URLClassLoader(
            scalaPBClasspath.map(_.toIO.toURI.toURL).toArray
          )
          val scalaPBCompilerClass = cl.loadClass("scalapb.ScalaPBC")
          val mainMethod = scalaPBCompilerClass.getMethod(
            "main",
            classOf[Array[java.lang.String]]
          )

          val instance = new ZScalaPBWorkerApi {
            override def compileScalaPB(
                source: File,
                scalaPBOptions: String,
                generatedDirectory: File,
                includes: Seq[os.Path],
                useZIO: Boolean,
                pluginArtifacts: Seq[String]
            ) {
              val opts =
                if (scalaPBOptions.isEmpty) "" else scalaPBOptions + ":"
              val args =
                pluginArtifacts.map(art => s"--plugin-artifact=$art") ++
                  protocPath.map(path => s"--protoc=$path").toSeq ++
                  Seq(
                    "--throw",
                    s"--scala_out=${opts}${generatedDirectory.getCanonicalPath}",
                    s"--proto_path=${source.getParentFile.getCanonicalPath}"
                  ) ++
                  (if (useZIO)
                     Seq(s"--zio_out=${generatedDirectory.getCanonicalPath}")
                   else Seq.empty) ++
                  includes.map(i => s"--proto_path=${i.toIO.getCanonicalPath}") :+
                  source.getCanonicalPath
              mainMethod.invoke(null, args.toArray)
            }
          }
          scalaPBInstanceCache = Some((classloaderSig, instance))
          instance
      }
    }

    def compile(
        scalaPBClasspath: Agg[os.Path],
        protocPath: Option[String],
        scalaPBSources: Seq[os.Path],
        scalaPBOptions: String,
        dest: os.Path,
        scalaPBIncludePath: Seq[os.Path],
        useZIO: Boolean,
        pluginArtifacts: Seq[String]
    )(implicit ctx: mill.api.Ctx): mill.api.Result[PathRef] = {
      val compiler = scalaPB(scalaPBClasspath, protocPath)

      def compileScalaPBDir(inputDir: os.Path) {
        // ls throws if the path doesn't exist
        if (inputDir.toIO.exists) {
          os.walk(inputDir)
            .filter(_.last.matches(".*.proto"))
            .foreach { proto =>
              compiler.compileScalaPB(
                proto.toIO,
                scalaPBOptions,
                dest.toIO,
                scalaPBIncludePath,
                useZIO,
                pluginArtifacts
              )
            }
        }
      }

      scalaPBSources.foreach(compileScalaPBDir)

      mill.api.Result.Success(PathRef(dest))
    }
  }

  trait ZScalaPBWorkerApi {
    def compileScalaPB(
        source: File,
        scalaPBOptions: String,
        generatedDirectory: File,
        includes: Seq[os.Path],
        useZIO: Boolean,
        pluginArtifacts: Seq[String]
    )
  }

  object ZScalaPBWorkerApi {

    def scalaPBWorker = new ZScalaPBWorker()
  }
}
