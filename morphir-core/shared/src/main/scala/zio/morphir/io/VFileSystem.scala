package zio.morphir.io
import zio.*

import java.io.IOException

/**
 * A `VFileSystem` provides a common environment for filesystem like operations.
 */
trait VFileSystem {
  def fileSeparator: FileSeparator
  def cwd: IO[IOException, VFilePath]
}

object VFileSystem extends VFileSystemPlatformSpecific with VFileSystemInstances {}

trait VFileSystemInstances { self: VFileSystem.type =>
  implicit val defaultVFileSystem: VFileSystem = LiveVFileSystem.default
}

final case class LiveVFileSystem(fileSeparator: FileSeparator, cwd: IO[IOException, VFilePath]) extends VFileSystem
object LiveVFileSystem {
  val default: LiveVFileSystem = {
    implicit val fileSeparator = FileSeparator(java.io.File.separator)
    val cwd = IO
      .attempt {
        val cwd = new java.io.File(".").getCanonicalPath
        VFilePath.fromString(cwd)
      }
      .refineOrDie { case e: java.io.IOException => e }

    LiveVFileSystem(fileSeparator, cwd)
  }

  def apply(cwd: IO[IOException, VFilePath]): LiveVFileSystem = {
    val fileSeparator = FileSeparator(java.io.File.separator)
    new LiveVFileSystem(fileSeparator, cwd)
  }
}
