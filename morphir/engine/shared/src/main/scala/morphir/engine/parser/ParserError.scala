package morphir.engine.parser

import java.io.IOException

import morphir.io.file.FsPath

sealed abstract class ParserError extends Throwable
object ParserError {
  type Message = String

  final case class VFileNotFound[B, T, S](path: FsPath[B, T, S], message: Message, cause: Option[Throwable] = None)
      extends IOException(message, cause.orNull) {

    def this(path: FsPath[B, T, S]) = {
      this(
        path,
        s"Could not find an actual file/directory at the path: ${FsPath.posixCodec.unsafePrintPath(path)}.",
        None
      )
    }
  }
}
