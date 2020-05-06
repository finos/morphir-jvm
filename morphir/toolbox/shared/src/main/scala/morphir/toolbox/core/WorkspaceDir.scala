package morphir.toolbox.core

import java.nio.file.{ Path, Paths }

import io.circe.{ Decoder, Encoder }
import zio._

import scala.util.Try

final case class WorkspaceDir private (path: Path) extends AnyVal {
  def /[R](segment: String): RIO[R, WorkspaceDir] =
    RIO.effect(Paths.get(path.toString, segment).toFile).flatMap { file =>
      if (file.isDirectory) Task.effect(WorkspaceDir(file.toPath))
      else
        RIO.fail(
          Errors.PathIsNotADirectoryError(
            file.toPath,
            s"The workspace directory must point at a directory, but $file is not a directory."
          )
        )
    }

  def join(segments: String*): UIO[Path] =
    ZIO.succeed(Paths.get(path.toString, segments: _*))

  def joinPath(segments: String*): Path =
    Paths.get(path.toString, segments: _*)

  def joinPath(segment: Path, rest: Path*): Path =
    Paths.get(path.toString, (segment :: rest.toList).map(_.toString): _*)

}

object WorkspaceDir {

  implicit val encodeWorkspaceDir: Encoder[WorkspaceDir] = Encoder.encodeString.contramap(dir => dir.toString)
  implicit val decodeWorkspaceDir: Decoder[WorkspaceDir] = Decoder.decodeString.emapTry(tryGet(_))

  def tryGet(segment: String, otherSegments: String*): Try[WorkspaceDir] =
    Try(Paths.get(segment, otherSegments: _*)).flatMap(tryMakeFromPath)

  def tryMakeFromPath(path: Path): Try[WorkspaceDir] =
    Try(path.toFile).flatMap { file =>
      if (file.isDirectory)
        scala.util.Success(WorkspaceDir(path))
      else
        scala.util.Failure(
          Errors.PathIsNotADirectoryError(
            file.toPath,
            s"The workspace directory must point at a directory, but $path is not a directory."
          )
        )
    }

  def fromPath[R](path: Path): Task[WorkspaceDir] =
    ZIO.fromTry(tryMakeFromPath(path))
}
