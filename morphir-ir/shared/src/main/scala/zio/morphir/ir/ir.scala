package zio.morphir.ir

import zio.morphir.ir.recursive.*
import zio.prelude._
import zio.prelude.fx._
import scala.collection.immutable._
import zio._
import zio.stm._

sealed trait TypeTree[+Annotations] extends IR[Annotations] { self =>
  import TypeTreeCase.*
  import DefinitionCase.*
  import SpecificationCase.*
  import TypeCase.*

  override def caseValue: TypeTreeCase[TypeTree[Annotations]]

  // def fold[Z](f: TypeTreeCase[Z] => Z): Z = self.caseValue match {
  //   case c @ ConstructorsCase(_) => f(ConstructorsCase(c.args.map { case (name, tree) => (name, tree.fold(f)) }))
  //   case c @ CustomTypeDefinitionCase(_, _)    => f(CustomTypeDefinitionCase(c.typeParams, c.ctors.map(_.fold(f))))
  //   case c @ TypeAliasDefinitionCase(_, _)     => f(TypeAliasDefinitionCase(c.typeParams, c.typeExpr.fold(f)))
  //   case c @ CustomTypeSpecificationCase(_, _) => f(CustomTypeSpecificationCase(c.typeParams, c.ctors.fold(f)))
  //   case c @ OpaqueTypeSpecificationCase(_)    => f(c)
  //   case c @ TypeAliasSpecificationCase(_, _)  => f(TypeAliasSpecificationCase(c.typeParams, c.typeExpr.fold(f)))
  //   case c @ ExtensibleRecordCase(_, _)        => f(ExtensibleRecordCase(c.name, c.fields.map(_.fold(f))))
  //   case c @ FunctionCase(_, _)                => f(FunctionCase(c.paramTypes.map(_.fold(f)), c.returnType.fold(f)))
  //   case c @ RecordCase(_)                     => f(RecordCase(c.fields.map(_.fold(f))))
  //   case c @ ReferenceCase(_, _)               => f(ReferenceCase(c.typeName, c.typeParams.map(_.fold(f))))
  //   case c @ TupleCase(_)                      => f(TupleCase(c.elementTypes.map(_.fold(f))))
  //   case UnitCase                              => f(UnitCase)
  //   case c @ VariableCase(_)                   => f(c)
  //   case c @ FieldCase(_, _)                   => f(FieldCase(c.name, c.fieldType.fold(f)))
  // }
}

object TypeTree {
  import TypeTreeCase.*
  import DefinitionCase.*
  import SpecificationCase.*
  final case class Constructors[+Annotations](
      args: Map[Name, Type[Annotations]],
      annotations: ZEnvironment[Annotations]
  ) extends TypeTree[Annotations] {
    override lazy val caseValue: TypeTreeCase[TypeTree[Annotations]] = ConstructorsCase(args)
  }

  sealed trait Definition[+Annotations] extends TypeTree[Annotations] { self =>
    def typeParams: List[Name]
    override def caseValue: DefinitionCase[TypeTree[Annotations]]
  }

  object Definition {
    object WithTypeParams {
      def unapply[Annotations](ir: IR[Annotations]): Option[(List[Name], DefinitionCase[TypeTree[Annotations]])] =
        ir match {
          case ir: Definition[Annotations] => Some((ir.typeParams, ir.caseValue))
          case _                           => None
        }
    }
    final case class CustomTypeDefinition[+Annotations](
        typeParams: List[Name],
        ctors: AccessControlled[Constructors[Annotations]],
        annotations: ZEnvironment[Annotations]
    ) extends Definition[Annotations] {
      override lazy val caseValue: DefinitionCase[TypeTree[Annotations]] = CustomTypeDefinitionCase(typeParams, ctors)
    }

    final case class TypeAliasDefinition[+Annotations](
        typeParams: List[Name],
        typeExpr: Type[Annotations],
        annotations: ZEnvironment[Annotations]
    ) extends Definition[Annotations] {
      override lazy val caseValue: DefinitionCase[TypeTree[Annotations]] = TypeAliasDefinitionCase(typeParams, typeExpr)
    }
  }

  sealed trait Specification[+Annotations] extends TypeTree[Annotations] { self =>
    def typeParams: List[Name]
    override def caseValue: SpecificationCase[TypeTree[Annotations]]
  }
  object Specification {
    def unapply[Annotations](t: Specification[Annotations]): Option[(SpecificationCase[TypeTree[Annotations]])] =
      Some(t.caseValue)

    object WithTypeParams {
      def unapply[Annotations](ir: IR[Annotations]): Option[(List[Name], SpecificationCase[TypeTree[Annotations]])] =
        ir match {
          case s: Specification[Annotations] => Some((s.typeParams, s.caseValue))
          case _                             => None
        }
    }

    final case class CustomTypeSpecification[+Annotations](
        typeParams: List[Name],
        ctors: Constructors[Annotations],
        annotations: ZEnvironment[Annotations]
    ) extends Specification[Annotations] {
      override lazy val caseValue: SpecificationCase[TypeTree[Annotations]] =
        CustomTypeSpecificationCase(typeParams, ctors)
    }
    final case class OpaqueTypeSpecification[+Annotations](
        typeParams: List[Name],
        annotations: ZEnvironment[Annotations]
    ) extends Specification[Annotations] {
      override lazy val caseValue: SpecificationCase[Type[Annotations]] = OpaqueTypeSpecificationCase(typeParams)
    }

    final case class TypeAliasSpecification[+Annotations](
        typeParams: List[Name],
        typeExpr: Type[Annotations],
        annotations: ZEnvironment[Annotations]
    ) extends Specification[Annotations] {
      override lazy val caseValue: SpecificationCase[Type[Annotations]] =
        TypeAliasSpecificationCase(typeParams, typeExpr)
    }
  }
}

sealed trait Type[+Annotations] extends TypeTree[Annotations] { self =>
  // import TypeCase.*

  final def asType: Type[Annotations] = self

  override def caseValue: TypeCase[Type[Annotations]]
}

object Type {
  import TypeCase.*

  def ref(name: naming.FQName): Reference[Any] = Reference(name, Chunk.empty, ZEnvironment.empty)

  /**
   * Creates a type variable with the given `name`.
   */
  def variable(name: Name): Variable[Any]   = Variable(name, ZEnvironment.empty)
  def variable(name: String): Variable[Any] = variable(Name(name))
  val unit: Type[Any]                       = UnitType(ZEnvironment.empty)

  final case class UnitType[+Annotations](annotations: ZEnvironment[Annotations]) extends Type[Annotations] {
    override val caseValue: TypeCase[Type[Annotations]] = UnitCase
  }

  final case class Field[+Annotations](name: Name, fieldType: Type[Annotations], annotations: ZEnvironment[Annotations])
      extends Type[Annotations] {
    override lazy val caseValue: FieldCase[Type[Annotations]] = FieldCase(name, fieldType)
  }
  object Field {

    object Case {
      def unapply[Annotations](field: Field[Annotations]): Option[FieldCase[Type[Annotations]]] =
        Some(field.caseValue)
    }
  }

  final case class Reference[+Annotations](
      name: FQName,
      typeParams: Chunk[Type[Annotations]],
      annotations: ZEnvironment[Annotations]
  ) extends Type[Annotations] {
    override lazy val caseValue: ReferenceCase[Type[Annotations]] = ReferenceCase(name, typeParams)
  }

  object Reference {
    object Case {
      def unapply[Annotations](reference: Reference[Annotations]): Option[ReferenceCase[Type[Annotations]]] =
        Some(reference.caseValue)
    }
  }

  final case class Variable[+Annotations](name: Name, annotations: ZEnvironment[Annotations])
      extends Type[Annotations] {
    override lazy val caseValue: VariableCase = VariableCase(name)
  }
  object Variable {
    object Case {
      def unapply[Annotations](variable: Variable[Annotations]): Option[VariableCase] =
        Some(variable.caseValue)
    }
  }
}

sealed trait ValueTree[+Annotations] extends IR[Annotations] { self =>
  import ValueTreeCase.*
  import ValueCase.*
  override def caseValue: ValueTreeCase[IR[Annotations]]
}

object ValueTree {
  import ValueTreeCase.*

  final case class Definition[+Annotations](
      inputTypes: Chunk[(Name, Type[Annotations])],
      outputType: Type[Annotations],
      body: Value[Annotations],
      annotations: ZEnvironment[Annotations]
  ) extends ValueTree[Annotations] {
    override def caseValue: ValueTreeCase[IR[Annotations]] = DefinitionCase(inputTypes, outputType, body)
  }

  final case class Specification[+Annotations](
      inputs: Chunk[(Name, Type[Annotations])],
      output: Type[Annotations],
      annotations: ZEnvironment[Annotations]
  ) extends ValueTree[Annotations] {
    override val caseValue: ValueTreeCase[IR[Annotations]] = SpecificationCase(inputs, output)
  }
}

sealed trait Value[+Annotations] extends ValueTree[Annotations] { self =>

  def caseValue: ValueCase[Value[Annotations]]
}

object Value {
  import ValueCase.*
  final case class Variable[+Annotations](name: Name, annotations: ZEnvironment[Annotations])
      extends Value[Annotations] {
    override lazy val caseValue: VariableCase = VariableCase(name)
  }
}

sealed trait Distribution[+Annotations] extends IR[Annotations] { self =>
  def caseValue: DistributionCase[IR[Annotations]]
}

object Distribution {
  import DistributionCase.*
  final case class Library[+Annotations](
      packageName: PackageName,
      packageSpecs: Map[PackageName, PackageSpecification[Annotations]],
      packageDef: PackageDefinition[Annotations],
      annotations: ZEnvironment[Annotations]
  ) extends Distribution[Annotations] {
    override def caseValue: LibraryCase[IR[Annotations]] = LibraryCase(packageName, packageSpecs, packageDef)
  }
}

final case class PackageSpecification[+Annotations](
    modules: scala.collection.immutable.Map[ModuleName, ModuleSpecification[Annotations]],
    annotations: ZEnvironment[Annotations]
) extends IR[Annotations] {
  override def caseValue: PackageSpecificationCase[ModuleSpecification[Annotations]] = PackageSpecificationCase(modules)
}

final case class PackageDefinition[+Annotations](
    modules: Map[ModuleName, AccessControlled[ModuleDefinition[Annotations]]],
    annotations: ZEnvironment[Annotations]
) extends IR[Annotations] {
  override def caseValue: PackageDefinitionCase[ModuleDefinition[Annotations]] = PackageDefinitionCase(modules)
}

final case class ModuleDefinition[+Annotations](
    types: Map[Name, AccessControlled[Documented[TypeTree.Definition[Annotations]]]],
    values: Map[Name, AccessControlled[ValueTree.Definition[Annotations]]],
    annotations: ZEnvironment[Annotations]
) extends IR[Annotations] {
  override def caseValue: ModuleDefinitionCase[IR[Annotations]] = ModuleDefinitionCase(types, values)
}

final case class ModuleSpecification[+Annotations](
    types: Map[Name, Documented[TypeTree.Specification[Annotations]]],
    values: Map[Name, ValueTree.Specification[Annotations]],
    annotations: ZEnvironment[Annotations]
) extends IR[Annotations] {
  override def caseValue: ModuleSpecificationCase[IR[Annotations]] = ModuleSpecificationCase(types, values)
}

sealed trait Literal[+A] {
  def value: A
}
object Literal {
  final case class Bool(value: scala.Boolean)               extends Literal[scala.Boolean]
  final case class Char(value: scala.Char)                  extends Literal[scala.Char]
  final case class String(value: java.lang.String)          extends Literal[java.lang.String]
  final case class WholeNumber(value: java.math.BigInteger) extends Literal[java.math.BigInteger]
  // TODO: Consider using BigDecimal as the representation of Float in Literal
  final case class Float(value: java.math.BigDecimal) extends Literal[java.math.BigDecimal]
}

sealed trait IR[+Annotations] { self =>
  import IRCase.*

  def caseValue: IRCase[IR[Annotations]]

  def annotations: ZEnvironment[Annotations]

  def fold[Z](f: IRCase[Z] => Z): Z =
    self.caseValue match {
      case c @ DistributionCase.LibraryCase(_, _, _) =>
        f(
          DistributionCase.LibraryCase(
            c.packageName,
            c.packageSpecs.map { case (name, spec) => (name, spec.fold(f)) },
            c.packageDef.fold(f)
          )
        )
      case c @ ModuleDefinitionCase(_, _) =>
        f(
          ModuleDefinitionCase(
            c.types.map { case (name, value) => (name, value.map(d => d.map(_.fold(f)))) },
            c.values.map { case (name, value) => (name, value.map(_.fold(f))) }
          )
        )
      case c @ ModuleSpecificationCase(_, _) =>
        f(
          ModuleSpecificationCase(
            c.types.map { case (name, value) => (name, value.map(_.fold(f))) },
            c.values.map { case (name, value) => (name, value.fold(f)) }
          )
        )
      case c @ PackageDefinitionCase(_) =>
        f(PackageDefinitionCase(c.modules.map { case (name, value) => (name, value.map(_.fold(f))) }))
      case c @ PackageSpecificationCase(_) =>
        f(PackageSpecificationCase(c.modules.map { case (name, spec) => (name, spec.fold(f)) }))
      case c @ PatternCase.AsCase(_, _) => f(PatternCase.AsCase(c.pattern.fold(f), c.name))
      case c @ PatternCase.ConstructorCase(_, _) =>
        f(PatternCase.ConstructorCase(c.constructorName, c.argumentPatterns.map(_.fold(f))))
      case c @ PatternCase.EmptyListCase      => f(PatternCase.EmptyListCase)
      case c @ PatternCase.HeadTailCase(_, _) => f(PatternCase.HeadTailCase(c.head.fold(f), c.tail.fold(f)))
      case c @ PatternCase.LiteralCase(_)     => f(PatternCase.LiteralCase(c.value))
      case c @ PatternCase.TupleCase(_)       => f(PatternCase.TupleCase(c.elements.map(_.fold(f))))
      case c @ PatternCase.UnitCase           => f(PatternCase.UnitCase)
      case c @ PatternCase.WildcardCase       => f(PatternCase.WildcardCase)
      case c @ ValueCase.ApplyCase(_, _)      => f(ValueCase.ApplyCase(c.function.fold(f), c.arguments.map(_.fold(f))))
      case c @ ValueCase.ConstructorCase(_) =>
        f(c)
        f(ValueCase.ConstructorCase(c.name))
      case c @ ValueCase.DestructureCase(_, _, _) =>
        f(ValueCase.DestructureCase(c.pattern.fold(f), c.valueToDestruct.fold(f), c.inValue.fold(f)))
      case c @ ValueCase.FieldCase(_, _)      => f(ValueCase.FieldCase(c.target.fold(f), c.name))
      case c @ ValueCase.FieldFunctionCase(_) => f(c)
      case c @ ValueCase.IfThenElseCase(_, _, _) =>
        f(ValueCase.IfThenElseCase(c.condition.fold(f), c.thenBranch.fold(f), c.elseBranch.fold(f)))
      case c @ ValueCase.LambdaCase(_, _) => f(ValueCase.LambdaCase(c.argumentPattern.fold(f), c.body.fold(f)))
      case c @ ValueCase.LetDefinitionCase(_, _, _) =>
        f(ValueCase.LetDefinitionCase(c.valueName, c.valueDefinition.fold(f), c.inValue.fold(f)))
      case c @ ValueCase.LetRecursionCase(_, _) =>
        f(
          ValueCase.LetRecursionCase(
            c.valueDefinitions.map { case (name, value) => (name, value.fold(f)) },
            c.inValue.fold(f)
          )
        )
      case c @ ValueCase.ListCase(_)    => f(ValueCase.ListCase(c.elements.map(_.fold(f))))
      case c @ ValueCase.LiteralCase(_) => f(c)
      case c @ ValueCase.PatternMatchCase(_, _) =>
        f(
          ValueCase.PatternMatchCase(
            c.branchOutOn.fold(f),
            c.cases.map { case (pattern, value) =>
              (pattern.fold(f), value.fold(f))
            }
          )
        )
      case c @ ValueCase.RecordCase(_)    => f(ValueCase.RecordCase(c.fields.map { case (k, v) => (k, v.fold(f)) }))
      case c @ ValueCase.ReferenceCase(_) => f(c)
      case c @ ValueCase.TupleCase(_)     => f(ValueCase.TupleCase(c.elements.map(_.fold(f))))
      case _ @ValueCase.UnitCase          => f(ValueCase.UnitCase)
      case c @ ValueCase.UpdateRecordCase(_, _) =>
        f(
          ValueCase.UpdateRecordCase(
            c.valueToUpdate.fold(f),
            c.fieldsToUpdate.map { case (name, value) => (name, value.fold(f)) }
          )
        )
      case c @ ValueCase.VariableCase(_)             => f(c)
      case c @ ValueTreeCase.DefinitionCase(_, _, _) => ???
      case c @ ValueTreeCase.SpecificationCase(_, _) =>
        f(
          ValueTreeCase.SpecificationCase(
            c.inputs.map { case (name, value) => (name, value.fold(f)) },
            c.output.fold(f)
          )
        )
      case c @ TypeCase.ExtensibleRecordCase(_, _) => f(TypeCase.ExtensibleRecordCase(c.name, c.fields.map(_.fold(f))))
      case c @ TypeCase.FieldCase(_, _)            => f(TypeCase.FieldCase(c.name, c.fieldType.fold(f)))
      case c @ TypeCase.FunctionCase(_, _) =>
        f(TypeCase.FunctionCase(c.paramTypes.map(_.fold(f)), c.returnType.fold(f)))
      case c @ TypeCase.RecordCase(_)       => f(TypeCase.RecordCase(c.fields.map(_.fold(f))))
      case c @ TypeCase.ReferenceCase(_, _) => f(TypeCase.ReferenceCase(c.typeName, c.typeParams.map(_.fold(f))))
      case c @ TypeCase.TupleCase(_)        => f(TypeCase.TupleCase(c.elementTypes.map(_.fold(f))))
      case c @ TypeCase.UnitCase            => f(TypeCase.UnitCase)
      case c @ TypeCase.VariableCase(_)     => f(c)
      case c @ TypeTreeCase.ConstructorsCase(_) =>
        f(TypeTreeCase.ConstructorsCase(c.args.map { case (name, tree) => (name, tree.fold(f)) }))
      case c @ TypeTreeCase.DefinitionCase.CustomTypeDefinitionCase(_, _) =>
        f(TypeTreeCase.DefinitionCase.CustomTypeDefinitionCase(c.typeParams, c.ctors.map(_.fold(f))))
      case c @ TypeTreeCase.DefinitionCase.TypeAliasDefinitionCase(_, _) =>
        f(TypeTreeCase.DefinitionCase.TypeAliasDefinitionCase(c.typeParams, c.typeExpr.fold(f)))
      case c @ TypeTreeCase.SpecificationCase.CustomTypeSpecificationCase(_, _) =>
        f(TypeTreeCase.SpecificationCase.CustomTypeSpecificationCase(c.typeParams, c.ctors.fold(f)))
      case c @ TypeTreeCase.SpecificationCase.OpaqueTypeSpecificationCase(_) => f(c)
      case c @ TypeTreeCase.SpecificationCase.TypeAliasSpecificationCase(_, _) =>
        f(TypeTreeCase.SpecificationCase.TypeAliasSpecificationCase(c.typeParams, c.typeExpr.fold(f)))
    }

  def foldDown[Z](z: Z)(f: (Z, IR[Annotations]) => Z): Z =
    caseValue.foldLeft(f(z, self))((z, recursive) => recursive.foldDown(z)(f))

  def foldDownSome[Z](z: Z)(pf: PartialFunction[(Z, IR[Annotations]), Z]): Z =
    foldDown(z)((z, recursive) => pf.lift(z -> recursive).getOrElse(z))

  def foldM[F[+_]: AssociativeFlatten: Covariant: IdentityBoth, Z](f: IRCase[Z] => F[Z]): F[Z] =
    fold[F[Z]](_.flip.flatMap(f))

  def foldPure[W, S, R, E, Z](f: IRCase[Z] => ZPure[W, S, S, R, E, Z]): ZPure[W, S, S, R, E, Z] =
    foldM(f)

  // TODO: Uncomment once appropriate instances are provided by ZIO Prelude

  // def foldManaged[R, E, Z](f: IRCase[Z] => ZManaged[R, E, Z]): ZManaged[R, E, Z] =
  //   foldM(f)

  // def foldSTM[R, E, Z](f: IRCase[Z] => ZSTM[R, E, Z]): ZSTM[R, E, Z] =
  //   foldM(f)

  // def foldValidation[W, E, Z](f: IRCase[Z] => ZValidation[W, E, Z]): ZValidation[W, E, Z] =
  //   foldM(f)

  def foldZIO[R, E, Z](f: IRCase[Z] => ZIO[R, E, Z]): ZIO[R, E, Z] =
    foldM(f)

  def foldRecursive[Z](f: IRCase[(IR[Annotations], Z)] => Z): Z =
    f(caseValue.map(recursive => recursive -> recursive.foldRecursive(f)))

  def foldUp[Z](z: Z)(f: (Z, IR[Annotations]) => Z): Z =
    f(caseValue.foldLeft(z)((z, recursive) => recursive.foldUp(z)(f)), self)

  def foldUpSome[Z](z: Z)(pf: PartialFunction[(Z, IR[Annotations]), Z]): Z =
    foldUp(z)((z, recursive) => pf.lift(z -> recursive).getOrElse(z))
}
object IR {}
// final case class Field[+A](name: Name, fieldType: TypeCase[A]) { self =>
//   def map[B](f: A => B): Field[B] = copy(fieldType = fieldType.map(f))
// }

// object Field {
//   implicit val FieldCovariant: Covariant[Field] = new Covariant[Field] {
//     def map[A, B](fa: Field[A])(f: A => B): Field[B] = fa.map(f)
//   }
// }

// final case class Fields[+A](items: zio.Chunk[Field[A]]) { self =>
//   def map[B](f: A => B): Fields[B] = copy(items = items.map(_.map(f)))
// }

// object Fields {
//   implicit val FieldsCovariant: Covariant[Fields] = new Covariant[Fields] {
//     def map[A, B](fa: Fields[A])(f: A => B): Fields[B] = fa.map(f)
//   }
// }
