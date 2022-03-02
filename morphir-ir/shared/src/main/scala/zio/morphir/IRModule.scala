package zio.morphir

import zio.Chunk
import zio.morphir.ir.*
import zio.morphir.ir.{TypeModule, ValueModule}
import zio.morphir.ir.TypeModule.UType

object IRModule {

  @inline final def fromDistribution(distribution: DistributionModule.Distribution): IR = ???

  @inline final def fromPackageSpecifications(specs: Map[FQName, PackageModule.Specification[Any]]): IR = ???

  @inline final def lookupTypeSpecification(fqName: FQName): IR.LookupTypeSpecification =
    IR.lookupTypeSpecification(fqName)

  @inline final def lookupTypeConstructor(fqName: FQName): IR.LookupTypeConstructor =
    IR.lookupTypeConstructor(fqName)

  final case class IR(
      valueSpecifications: Map[FQName, ValueModule.Specification[Any]],
      valueDefinitions: Map[FQName, ValueModule.ValueDefinition[UType]],
      typeSpecifications: Map[FQName, TypeModule.Specification[Any]],
      typeConstructors: Map[FQName, TypeConstructorInfo]
  ) { self =>

    @inline final def lookupTypeSpecification(fqName: FQName): Option[TypeModule.Specification[Any]] =
      typeSpecifications.get(fqName)

    @inline final def lookupTypeConstructor(fqName: FQName): Option[TypeConstructorInfo] =
      typeConstructors.get(fqName)

    def resolveAliases(fqName: FQName): FQName =
      typeSpecifications.get(fqName) match {
        case Some(typeSpecification) =>
          typeSpecification match {
            case TypeModule.Specification.TypeAliasSpecification(_, underlyingType, _) =>
              underlyingType.caseValue match {
                case TypeModule.TypeCase.ReferenceCase(fqName, _) =>
                  fqName
                case _ => fqName
              }
            case _ => fqName
          }
        case None => fqName
      }
  }

  object IR {
    val empty: IR = IR(
      valueSpecifications = Map.empty,
      valueDefinitions = Map.empty,
      typeSpecifications = Map.empty,
      typeConstructors = Map.empty
    )

    def fromDistribution(distribution: DistributionModule.Distribution): IR = ???

    def fromPackageSpecifications(specs: Map[FQName, PackageModule.Specification[Any]]): IR = ???

    def lookupTypeSpecification(fqName: FQName): LookupTypeSpecification = new LookupTypeSpecification(() => fqName)

    def lookupTypeConstructor(fqName: FQName): LookupTypeConstructor = new LookupTypeConstructor(() => fqName)

    final class LookupTypeSpecification(val fqName: () => FQName) extends AnyVal {
      def apply(ir: IR): Option[TypeModule.Specification[Any]] =
        ir.lookupTypeSpecification(fqName())
    }

    final class LookupTypeConstructor(val fqName: () => FQName) extends AnyVal {
      def apply(ir: IR): Option[TypeConstructorInfo] =
        ir.lookupTypeConstructor(fqName())
    }
  }

  final case class TypeConstructorInfo(containingType: FQName, typeParams: Chunk[Name], typeArgs: Chunk[(Name, UType)])
}
