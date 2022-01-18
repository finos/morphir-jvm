package zio.morphir.io

trait HasFileSystem[A] {
  def fileSystem: VFileSystem
}

object HasFileSystem extends HasFileSystemLowerPriority {
  implicit def apply[A](implicit ev: HasFileSystem[A]): HasFileSystem[A] = ev
  def apply[A](provide: => VFileSystem): HasFileSystem[A] = new HasFileSystem[A] {
    override def fileSystem: VFileSystem = provide
  }
}

trait HasFileSystemLowerPriority {
  implicit def fallbackHasFileSystem[A]: HasFileSystem[A] = HasFileSystem {
    VFileSystem.defaultVFileSystem
  }
}
