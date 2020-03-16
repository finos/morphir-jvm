package morphir.internal.io.file

import java.io.IOException
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files => JFiles, Path => JPath}
import java.util.{Iterator => JIterator}

import zio.blocking._
import zio.stream.ZStream
import zio.{Chunk, UIO, ZIO, ZManaged}

import scala.jdk.CollectionConverters._

object Files {
  def readAllBytes(path: JPath): ZIO[Blocking, IOException, Chunk[Byte]] =
    effectBlocking(Chunk.fromArray(JFiles.readAllBytes(path)))
      .refineToOrDie[IOException]

  def readAllLines(
      path: JPath,
      charset: Charset = StandardCharsets.UTF_8
  ): ZIO[Blocking, IOException, List[String]] =
    effectBlocking(JFiles.readAllLines(path, charset).asScala.toList)
      .refineToOrDie[IOException]
}
