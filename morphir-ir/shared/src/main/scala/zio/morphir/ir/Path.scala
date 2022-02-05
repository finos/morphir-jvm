package zio.morphir.ir
import zio.Chunk
import zio.morphir.ir.PackageModule.PackageAndModulePath

final case class Path(segments: Chunk[Name]) { self =>

  /** Constructs a new path by combining this path with the given name. */
  def /(name: Name): Path = Path(segments ++ Chunk(name))

  /** Constructs a new path by combining this path with the given path. */
  def /(name: Path): Path = Path(segments ++ name.segments)
  def %(other: Path): PackageAndModulePath =
    PackageAndModulePath(PackageName(self), ModulePath(other))

  def %(name: Name): ModuleName = ModuleName(self, name)

  /** Indicates whether this path is empty. */
  def isEmpty: Boolean               = segments.isEmpty
  def zip(other: Path): (Path, Path) = (self, other)

}

object Path {
  val empty: Path = Path(Chunk.empty)
}
