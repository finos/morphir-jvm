package morphir.toolbox.core

import java.io.{ File, IOException }
import java.nio.charset.StandardCharsets
import java.nio.file.Paths

import morphir.core.newtype._
import morphir.toolbox.workspace.config.WorkspaceSettings
import zio.blocking.Blocking
import zio.nio.core.file.Path
import zio.nio.file.Files
import zio.{ Chunk, RIO, UIO, ZIO }

object ManifestFile extends Newtype[File] {
  val DefaultName: String = "morphir.toml"

  def fromDirectory(
    workspaceDir: WorkspaceDir
  ): ZIO[Any, Throwable, ManifestFile] =
    workspaceDir
      .join(DefaultName)
      .flatMap(path => ZIO.effect(ManifestFile(path.toFile)))

  def fromFile(file: File): UIO[ManifestFile] =
    ZIO.succeed(ManifestFile(file))

  def fromPath[R](workspacePath: WorkspacePath): RIO[R, ManifestFile] =
    ZIO.effect(workspacePath.toFile).flatMap { file =>
      if (file.isDirectory)
        ZIO.effect(
          ManifestFile(Paths.get(file.getPath, DefaultName).toFile)
        )
      else ZIO.effect(ManifestFile(file))
    }

  implicit class RichManifestFile(val manifestFile: ManifestFile) extends AnyVal {

    def toPath: UIO[Path] = ZIO.effectTotal {
      val file = ManifestFile.unwrap(manifestFile)
      Path.fromJava(file.toPath)
    }

    def readAllBytes: ZIO[Blocking, IOException, Chunk[Byte]] =
      for {
        path <- toPath
        data <- Files.readAllBytes(path)
      } yield data

    def readAllText: ZIO[Blocking, IOException, String] =
      readAllBytes.map(chunk => new String(chunk.toArray, StandardCharsets.UTF_8))

    def load: ZIO[Blocking, Throwable, WorkspaceSettings] =
      for {
        contents     <- readAllText
        path         <- toPath
        absolutePath <- path.toAbsolutePath
        settings <- ZIO
                     .fromEither(WorkspaceSettings.fromToml(contents))
                     .mapError { e =>
                       val (_, message) = e
                       Errors.ManifestFileParseError(
                         e,
                         s"""Failed to parse the morphir workspace manifest file at: $absolutePath.
                            |Cause:
                            |$message
                            |Contents:
                            |$contents""".stripMargin
                       )
                     }
      } yield settings
  }
}
