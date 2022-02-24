package zio.morphir.ir

object ModuleModule {

  final case class Definition[+Annotations](
      types: Map[Name, AccessControlled[Documented[TypeModule.Definition[Annotations]]]],
      values: Map[Name, AccessControlled[ValueModule.Definition[Annotations]]]
  ) { self =>
    def toSpecification: Specification[Annotations] = {
      Specification(
        types = self.types.collect { case (name, AccessControlled.WithPublicAccess(documented)) =>
          name -> documented.map(_.toSpecification)
        },
        values = self.values.collect { case (name, AccessControlled.WithPublicAccess(definition)) =>
          name -> definition.toSpecification
        }
      )
    }

    def toSpecificationWithPrivate: Specification[Annotations] = {
      Specification(
        types = self.types.collect { case (name, AccessControlled.WithPrivateAccess(documented)) =>
          name -> documented.map(_.toSpecification)
        },
        values = self.values.collect { case (name, AccessControlled.WithPrivateAccess(definition)) =>
          name -> definition.toSpecification
        }
      )
    }

    def lookupValue(localName: Name): Option[ValueModule.Definition[Annotations]] = {
      values.get(localName).flatMap(x => AccessControlled.WithPrivateAccess.unapply(x))
    }

    def eraseAttributes: Definition[Annotations] = Definition.empty

    def mapAttributes: Definition[Annotations] = ???

    def collectTypeReferences: Set[FQName]         = ???
    def collectValueReferences: Set[FQName]        = ???
    def collectReferences: Set[FQName]             = ???
    def dependsOnModules: Set[QualifiedModuleName] = ???
  }

  object Definition {
    def empty[Annotations]: Definition[Annotations] = Definition(Map.empty, Map.empty)
  }

  final case class Specification[+Annotations](
      types: Map[Name, Documented[TypeModule.Specification[Annotations]]],
      values: Map[Name, ValueModule.Specification[Annotations]]
  ) {
    def lookupValue(localName: Name): Option[ValueModule.Specification[Annotations]] = values.get(localName)
    def lookupType(localName: Name): Option[TypeModule.Specification[Annotations]] =
      types.get(localName).map(doc => doc.value)

    def eraseAttributes: Specification[Annotations] = Specification.empty

    def mapAttributes: Specification[Annotations] = ???
  }

  object Specification {
    def empty[Annotations]: Specification[Annotations] = Specification(Map.empty, Map.empty)
  }

  lazy val emptyDefinition: Definition[Any] = Definition.empty

  lazy val emptySpecification: Specification[Any] = Specification.empty

  final case class ModuleName(namespace: Path, localName: Name) {
    def %(name: Name): QName = QName(toPath, name)

    lazy val toPath = namespace / localName
  }

  final case class ModulePath(toPath: Path) extends AnyVal

  final case class QualifiedModuleName(packageName: Path, module: Path) {
    lazy val toPath = packageName / module
  }

}

trait ModuleSpecFor[A] {
  import ModuleModule.*

  def module: ModuleName
  def spec: Specification[Any]
}

object ModuleSpecFor {
  import ModuleModule.*

  /** Summon the module specification for the given module/type. */
  def apply[A](implicit specFor: ModuleSpecFor[A]): ModuleSpecFor[A] = specFor

  def make[A](name: ModuleName)(moduleSpec: Specification[Any]): ModuleSpecFor[A] =
    new ModuleSpecFor[A] {
      val module = name
      val spec   = moduleSpec
    }
}
