package zio.morphir

import zio.Chunk
import zio.morphir.ir.{Distribution, FQName, Name, PackageModule, Value}
import zio.morphir.ir.types.UType
import zio.morphir.IR.TypeConstructorInfo
import zio.morphir.ir.Type.{Type, Specification}

final case class IR(
    valueSpecifications: Map[FQName, Value.Specification.Raw],
    valueDefinitions: Map[FQName, Value.Definition.Typed],
    typeSpecifications: Map[FQName, Specification[Any]],
    typeConstructors: Map[FQName, TypeConstructorInfo]
) { self =>

  @inline final def lookupTypeSpecification(fqName: FQName): Option[Specification[Any]] =
    typeSpecifications.get(fqName)

  @inline final def lookupTypeConstructor(fqName: FQName): Option[TypeConstructorInfo] =
    typeConstructors.get(fqName)

  def resolveAliases(fqName: FQName): FQName =
    typeSpecifications.get(fqName) match {
      case Some(typeSpecification) =>
        typeSpecification match {
          case Specification.TypeAliasSpecification(_, underlyingType) =>
            underlyingType match {
              case Type.Reference(_, fqName, _) =>
                fqName
              case _ => fqName
            }
          case _ => fqName
        }
      case None => fqName
    }
}
object IR {

  @inline final def fromDistribution(distribution: Distribution.Distribution): IR = ???

  @inline final def fromPackageSpecifications(specs: Map[FQName, PackageModule.Specification[Any]]): IR = ???

  @inline final def lookupTypeSpecification(fqName: FQName): IR.LookupTypeSpecification =
    IR.lookupTypeSpecification(fqName)

  @inline final def lookupTypeConstructor(fqName: FQName): IR.LookupTypeConstructor =
    IR.lookupTypeConstructor(fqName)

  val empty: IR = IR(
    valueSpecifications = Map.empty,
    valueDefinitions = Map.empty,
    typeSpecifications = Map.empty,
    typeConstructors = Map.empty
  )

  final class LookupTypeSpecification(val fqName: () => FQName) extends AnyVal {
    def apply(ir: IR): Option[Specification[Any]] =
      ir.lookupTypeSpecification(fqName())
  }

  final class LookupTypeConstructor(val fqName: () => FQName) extends AnyVal {
    def apply(ir: IR): Option[TypeConstructorInfo] =
      ir.lookupTypeConstructor(fqName())
  }
  final case class TypeConstructorInfo(containingType: FQName, typeParams: Chunk[Name], typeArgs: Chunk[(Name, UType)])
}
