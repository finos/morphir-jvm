package zio.morphir.ir

object Module {

  type Definition[+Annotations] = MorphirIR.ModuleDefinition[Annotations]
  val Definition = MorphirIR.ModuleDefinition

  type Specification[+Annotations] = MorphirIR.ModuleSpecification[Annotations]
  val Specification = MorphirIR.ModuleSpecification

  lazy val emptyDefinition: Definition[Any] = Definition.empty

  lazy val emptySpecification: Specification[Any] = Specification.empty

  final case class ModuleName(namespace: Path, localName: Name) {
    lazy val toPath = namespace / localName
  }

  final case class ModulePath(toPath: Path)

  final case class QualifiedModuleName(packageName: Path, module: Path) {
    lazy val toPath = packageName / module
  }

}

trait ModuleSpecFor[A] {
  import Module.*

  def module: ModuleName
  def spec: Specification[Any]
}

object ModuleSpecFor {
  import Module.*

  /** Summon the module specification for the given module/type. */
  def apply[A](implicit specFor: ModuleSpecFor[A]): ModuleSpecFor[A] = specFor

  def make[A](name: ModuleName)(moduleSpec: Specification[Any]): ModuleSpecFor[A] =
    new ModuleSpecFor[A] {
      val module = name
      val spec   = moduleSpec
    }
}
