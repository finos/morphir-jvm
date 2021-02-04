package morphir.flowz

import zio.Chunk

final case class NodePath(segments: Chunk[String])
object NodePath {}
