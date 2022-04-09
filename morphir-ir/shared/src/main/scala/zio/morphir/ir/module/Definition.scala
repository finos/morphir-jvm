package zio.morphir.ir.module

import zio.morphir.ir.{AccessControlled, Documented, FQName, Name, Type, Value}

import Type.Definition.{CustomType, TypeAlias}
final case class Definition[+TA, +VA](
    types: Map[Name, AccessControlled[Documented[Type.Definition[TA]]]],
    values: Map[Name, AccessControlled[Documented[Value.Definition[TA, VA]]]]
) { self =>
  def toSpecification: Specification[TA] =
    Specification(
      types = self.types.collect { case (name, AccessControlled.WithPublicAccess(documented)) =>
        name -> documented.map(_.toSpecification)
      },
      values = self.values.collect { case (name, AccessControlled.WithPublicAccess(definition)) =>
        name -> definition.map(_.toSpecification)
      }
    )

  def toSpecificationWithPrivate: Specification[TA] =
    Specification(
      types = self.types.collect { case (name, AccessControlled.WithPrivateAccess(documented)) =>
        name -> documented.map(_.toSpecification)
      },
      values = self.values.collect { case (name, AccessControlled.WithPrivateAccess(documented)) =>
        name -> documented.map(_.toSpecification)
      }
    )

  def lookupValueDefinition(localName: Name): Option[Value.Definition[TA, VA]] =
    values.get(localName).flatMap(x => AccessControlled.WithPrivateAccess.unapply(x).map(_.value))

  def eraseAttributes: Definition[Any, Any] = ???

  def mapAttributes[TB, VB](tf: TA => TB, vf: VA => VB): Definition[TA, VA] = ???

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
    case (_, AccessControlled.WithPrivateAccess(documented)) =>
      documented.value.body.collectReferences
    case (_, AccessControlled.WithPublicAccess(documented)) =>
      documented.value.body.collectReferences
    case _ => Nil
  }.toSet

  def collectReferences: Set[FQName] = collectTypeReferences ++ collectValueReferences
  def dependsOnModules: Set[QualifiedModuleName] = self.collectReferences.map { case FQName(pp, mp, _) =>
    QualifiedModuleName(pp.toPath, mp.toPath)
  }
}

object Definition {
  def empty: Definition[Nothing, Nothing] = Definition(Map.empty, Map.empty)
}
