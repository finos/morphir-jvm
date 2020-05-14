package morphir.io.file

import java.net.URI

sealed trait VFilePath {
  def segments: List[String]
  def rawString: String
  def toURI: URI                = URI.create(rawString)
  override def toString: String = rawString
}

object VFilePath {

  final case class ResourcePath(segments: List[String]) extends VFilePath {
    def rawString: String = segments.mkString("resource:/", "/", "")
  }

  final case class FilePath(segments: List[String]) extends VFilePath {
    def rawString: String = segments.mkString("file:/", "/", "")
  }

  def forResource(path: String): VFilePath = {
    val segments = path.split(Array('/'))
    ResourcePath(segments.toList)
  }

  def of(segment: String, otherSegments: String*): VFilePath =
    FilePath(segment :: otherSegments.toList)
}
