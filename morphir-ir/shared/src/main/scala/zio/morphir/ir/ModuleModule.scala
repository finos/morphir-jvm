package zio.morphir.ir

import zio.Chunk
import zio.morphir.ir.TypeModule.Definition.{CustomType, TypeAlias}

object ModuleModule {

  final case class Definition[+Annotations](
      types: Map[Name, AccessControlled[Documented[TypeModule.Definition[Annotations]]]],
      values: Map[Name, AccessControlled[ValueModule.ValueDefinition[Annotations]]]
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

    def lookupValue(localName: Name): Option[ValueModule.ValueDefinition[Annotations]] = {
      values.get(localName).flatMap(x => AccessControlled.WithPrivateAccess.unapply(x))
    }

    def eraseAttributes: Definition[Annotations] = Definition.empty

    def mapAttributes: Definition[Annotations] = ???

    def collectTypeReferences: Set[FQName] = self.types.flatMap {
      case (_, AccessControlled.WithPrivateAccess(definition)) =>
        definition.value match {
          case TypeAlias(_, typeExp) => typeExp.collectReferences
          case CustomType(_, ctors)  => ctors.withPrivateAccess.collectReferences
        }
      case (_, AccessControlled.WithPublicAccess(definition)) =>
        definition.value match {
          case TypeAlias(_, typeExp) => typeExp.collectReferences
          case CustomType(_, ctors)  => ctors.withPrivateAccess.collectReferences
        }
      case _ => Nil

    }.toSet

    def collectValueReferences: Set[FQName] = self.values.flatMap {
      case (_, AccessControlled.WithPrivateAccess(definition)) =>
        definition.body.collectReferences
      case (_, AccessControlled.WithPublicAccess(definition)) =>
        definition.body.collectReferences
      case _ => Nil
    }.toSet

    def collectReferences: Set[FQName] = collectTypeReferences ++ collectValueReferences
    def dependsOnModules: Set[QualifiedModuleName] = self.collectReferences.map { case FQName(pp, mp, _) =>
      QualifiedModuleName(pp.toPath, mp.toPath)
    }
  }

  object Definition {
    def empty[Annotations]: Definition[Annotations] = Definition(Map.empty, Map.empty)
  }

  type USpecification = Specification[Any]
  val USpecification = Specification

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

    def toModulePath: ModulePath = ModulePath(toPath)

    lazy val toPath = namespace / localName
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

  final case class ModulePath(toPath: Path) extends AnyVal

  final case class QualifiedModuleName(packageName: Path, module: Path) {
    lazy val toPath = packageName / module
  }

}

trait ModuleSpecFor[A] {
  import ModuleModule._

  def module: ModuleName
  def spec: Specification[Any]
}

object ModuleSpecFor {
  import ModuleModule._

  /** Summon the module specification for the given module/type. */
  def apply[A](implicit specFor: ModuleSpecFor[A]): ModuleSpecFor[A] = specFor

  def make[A](name: ModuleName)(moduleSpec: Specification[Any]): ModuleSpecFor[A] =
    new ModuleSpecFor[A] {
      val module = name
      val spec   = moduleSpec
    }
}
