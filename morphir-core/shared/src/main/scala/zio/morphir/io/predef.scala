package zio.morphir.io
import zio.prelude.*
object predef {
  object FileSeparator extends Subtype[String] {
    val default: Type = wrap(java.io.File.separator)
  }
  type FileSeparator = FileSeparator.Type
}
