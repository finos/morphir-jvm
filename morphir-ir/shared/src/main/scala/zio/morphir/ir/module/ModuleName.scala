package zio.morphir.ir.module
import zio.Chunk
import zio.morphir.ir.{Name, Path, QName}

final case class ModuleName(namespace: Path, localName: Name) {
  def %(name: Name): QName = QName(toPath, name)

  def toModulePath: ModulePath = ModulePath(toPath)

  lazy val toPath: Path         = namespace / localName
  override def toString: String = toPath.toString
}

object ModuleName {
  def fromPath(path: Path): ModuleName = path.segments match {
    case Chunk()     => ModuleName(Path.empty, Name.empty)
    case Chunk(name) => ModuleName(Path.empty, name)
    case ns :+ name  => ModuleName(Path(ns), name)
    case names =>
      val ns   = names.take(names.length - 1)
      val name = names.last
      ModuleName(Path(ns), name)
  }

  def fromString(input: String): ModuleName = fromPath(Path.fromString(input))

  private[morphir] def unsafeMake(namespace: String*)(nameSegments: String*): ModuleName = {
    val ns        = namespace.foldLeft(Path.empty) { case (path, pathStr) => path / Path.fromString(pathStr) }
    val localName = Name.unsafeMake(nameSegments: _*)
    ModuleName(ns, localName)
  }
}
