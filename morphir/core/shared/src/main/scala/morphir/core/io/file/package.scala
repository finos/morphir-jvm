package morphir.core.io

package object file {
  type FsPath[+B, +T, +S] = FileSystemPath[B, T, S]
  val FsPath: FileSystemPath.type = FileSystemPath

  type UnrestrictedFsPath[+S] = FileSystemPath[Any, Any, S]
  type FilePath[+S]           = FileSystemPath[Any, FileSystemPath.File, S]
}
