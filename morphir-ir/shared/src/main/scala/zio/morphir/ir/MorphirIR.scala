package zio.morphir.ir

import zio.morphir.ir.{Literal => Lit}
import zio.morphir.ir.recursive.*
import zio.prelude._
import zio.prelude.fx._
import scala.collection.immutable._
import zio._

sealed trait MorphirIR[+Annotations] { self =>
  import MorphirIRCase.*

  def caseValue: MorphirIRCase[MorphirIR[Annotations]]

  def annotations: ZEnvironment[Annotations]

  def fold[Z](f: MorphirIRCase[Z] => Z): Z =
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
      case _ @PatternCase.EmptyListCase       => f(PatternCase.EmptyListCase)
      case c @ PatternCase.HeadTailCase(_, _) => f(PatternCase.HeadTailCase(c.head.fold(f), c.tail.fold(f)))
      case c @ PatternCase.LiteralCase(_)     => f(PatternCase.LiteralCase(c.value))
      case c @ PatternCase.TupleCase(_)       => f(PatternCase.TupleCase(c.elements.map(_.fold(f))))
      case _ @PatternCase.UnitCase            => f(PatternCase.UnitCase)
      case _ @PatternCase.WildcardCase        => f(PatternCase.WildcardCase)
      case c @ ValueCase.ApplyCase(_, _)      => f(ValueCase.ApplyCase(c.function.fold(f), c.arguments.map(_.fold(f))))
      case c @ ValueCase.ConstructorCase(_)   => f(ValueCase.ConstructorCase(c.name))
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
      case c @ ValueCase.ListCase(_)           => f(ValueCase.ListCase(c.elements.map(_.fold(f))))
      case c @ ValueCase.LiteralCase(_)        => f(c)
      case c @ ValueCase.NativeApplyCase(_, _) => f(ValueCase.NativeApplyCase(c.function, c.arguments.map(_.fold(f))))
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
      case c @ ValueCase.VariableCase(_) => f(c)
      case c @ ValueTreeCase.DefinitionCase(_, _, _) =>
        f(
          ValueTreeCase.DefinitionCase(
            c.inputTypes.map { case (name, self) => (name, self.fold(f)) },
            c.outputType.fold(f),
            c.body.fold(f)
          )
        )
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
      case _ @TypeCase.UnitCase             => f(TypeCase.UnitCase)
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

  def foldDown[Z](z: Z)(f: (Z, MorphirIR[Annotations]) => Z): Z =
    caseValue.foldLeft(f(z, self))((z, recursive) => recursive.foldDown(z)(f))

  def foldDownSome[Z](z: Z)(pf: PartialFunction[(Z, MorphirIR[Annotations]), Z]): Z =
    foldDown(z)((z, recursive) => pf.lift(z -> recursive).getOrElse(z))

  def foldM[F[+_]: AssociativeFlatten: Covariant: IdentityBoth, Z](f: MorphirIRCase[Z] => F[Z]): F[Z] =
    fold[F[Z]](_.flip.flatMap(f))

  def foldPure[W, S, R, E, Z](f: MorphirIRCase[Z] => ZPure[W, S, S, R, E, Z]): ZPure[W, S, S, R, E, Z] =
    foldM(f)

  // TODO: Uncomment once appropriate instances are provided by ZIO Prelude

  // def foldManaged[R, E, Z](f: IRCase[Z] => ZManaged[R, E, Z]): ZManaged[R, E, Z] =
  //   foldM(f)

  // def foldSTM[R, E, Z](f: IRCase[Z] => ZSTM[R, E, Z]): ZSTM[R, E, Z] =
  //   foldM(f)

  // def foldValidation[W, E, Z](f: IRCase[Z] => ZValidation[W, E, Z]): ZValidation[W, E, Z] =
  //   foldM(f)

  def foldZIO[R, E, Z](f: MorphirIRCase[Z] => ZIO[R, E, Z]): ZIO[R, E, Z] =
    foldM(f)

  def foldRecursive[Z](f: MorphirIRCase[(MorphirIR[Annotations], Z)] => Z): Z =
    f(caseValue.map(recursive => recursive -> recursive.foldRecursive(f)))

  def foldUp[Z](z: Z)(f: (Z, MorphirIR[Annotations]) => Z): Z =
    f(caseValue.foldLeft(z)((z, recursive) => recursive.foldUp(z)(f)), self)

  def foldUpSome[Z](z: Z)(pf: PartialFunction[(Z, MorphirIR[Annotations]), Z]): Z =
    foldUp(z)((z, recursive) => pf.lift(z -> recursive).getOrElse(z))

  def transformDown[Annotations0 >: Annotations](
      f: MorphirIR[Annotations0] => MorphirIR[Annotations0]
  ): MorphirIR[Annotations0] = {
    def loop(recursive: MorphirIR[Annotations0]): MorphirIR[Annotations] =
      MorphirIR(f(recursive).caseValue.map(loop), annotations)
    loop(self)
  }

  def transformDownSome[Annotations0 >: Annotations](
      pf: PartialFunction[MorphirIR[Annotations0], MorphirIR[Annotations0]]
  ): MorphirIR[Annotations0] =
    transformDown[Annotations0]((recursive => pf.lift(recursive).getOrElse(recursive)))

  def transformUp[Annotations0 >: Annotations](
      f: MorphirIR[Annotations0] => MorphirIR[Annotations0]
  ): MorphirIR[Annotations0] = {
    def loop(recursive: MorphirIR[Annotations0]): MorphirIR[Annotations0] =
      f(MorphirIR(recursive.caseValue.map(loop), annotations))
    loop(self)
  }

  def transformUpSome[Annotations0 >: Annotations](
      pf: PartialFunction[MorphirIR[Annotations0], MorphirIR[Annotations0]]
  ): MorphirIR[Annotations0] =
    transformUp[Annotations0]((recursive => pf.lift(recursive).getOrElse(recursive)))

}

object MorphirIR {

  def apply(
      caseValue0: MorphirIRCase[MorphirIR[Any]]
  ): MorphirIR[Any] =
    new MorphirIR[Any] {
      def caseValue   = caseValue0
      def annotations = ZEnvironment.empty
    }

  def apply[Annotations](
      caseValue0: MorphirIRCase[MorphirIR[Annotations]],
      annotations0: ZEnvironment[Annotations]
  ): MorphirIR[Annotations] =
    new MorphirIR[Annotations] {
      def caseValue   = caseValue0
      def annotations = annotations0
    }

  def unapply[Annotations](
      morphir: MorphirIR[Annotations]
  ): Option[(MorphirIRCase[MorphirIR[Annotations]], ZEnvironment[Annotations])] =
    Some((morphir.caseValue, morphir.annotations))

  sealed trait TypeTree[+Annotations] extends MorphirIR[Annotations] { self =>
    override def caseValue: TypeTreeCase[TypeTree[Annotations]]
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

        def unapply[Annotations](
            ir: MorphirIR[Annotations]
        ): Option[(List[Name], DefinitionCase[TypeTree[Annotations]])] =
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
        override lazy val caseValue: DefinitionCase[TypeTree[Annotations]] =
          TypeAliasDefinitionCase(typeParams, typeExpr)
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
        def unapply[Annotations](
            ir: MorphirIR[Annotations]
        ): Option[(List[Name], SpecificationCase[TypeTree[Annotations]])] =
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

    def transformDown[Annotations0 >: Annotations](
        f: Type[Annotations0] => Type[Annotations0]
    ): Type[Annotations0] = {
      def loop(recursive: Type[Annotations0]): Type[Annotations] =
        Type(f(recursive).caseValue.map(loop), annotations)
      loop(self)
    }
  }

  object Type {
    import TypeCase.*

    def apply[Annotations](
        caseValue0: TypeCase[Type[Annotations]],
        annotations0: ZEnvironment[Annotations]
    ): Type[Annotations] =
      new Type[Annotations] {
        override def caseValue: TypeCase[Type[Annotations]] = caseValue0
        override def annotations: ZEnvironment[Annotations] = annotations0
      }

    def ref(name: FQName): Reference[Any] = Reference(name, Chunk.empty, ZEnvironment.empty)

    /**
     * Creates a type variable with the given `name`.
     */
    def variable(name: Name): Variable[Any]   = Variable(name, ZEnvironment.empty)
    def variable(name: String): Variable[Any] = variable(Name(name))
    val unit: Type[Any]                       = Unit(ZEnvironment.empty)

    final case class Unit[+Annotations](annotations: ZEnvironment[Annotations]) extends Type[Annotations] {
      override val caseValue: TypeCase[Type[Annotations]] = UnitCase
    }

    final case class Field[+Annotations](
        name: Name,
        fieldType: Type[Annotations],
        annotations: ZEnvironment[Annotations]
    ) extends Type[Annotations] {
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

  sealed trait ValueTree[+Annotations] extends MorphirIR[Annotations] { self =>
    override def caseValue: ValueTreeCase[MorphirIR[Annotations]]
  }

  object ValueTree {
    import ValueTreeCase.*

    final case class Definition[+Annotations](
        inputTypes: Chunk[(Name, Type[Annotations])],
        outputType: Type[Annotations],
        body: Value[Annotations],
        annotations: ZEnvironment[Annotations]
    ) extends ValueTree[Annotations] {
      override def caseValue: ValueTreeCase[MorphirIR[Annotations]] = DefinitionCase(inputTypes, outputType, body)
    }

    final case class Specification[+Annotations](
        inputs: Chunk[(Name, Type[Annotations])],
        output: Type[Annotations],
        annotations: ZEnvironment[Annotations]
    ) extends ValueTree[Annotations] {
      override val caseValue: ValueTreeCase[MorphirIR[Annotations]] = SpecificationCase(inputs, output)
    }
  }

  sealed trait Value[+Annotations] extends ValueTree[Annotations] { self =>

    def caseValue: ValueCase[Value[Annotations]]
  }

  object Value {
    def apply(
        caseValue0: ValueCase[Value[Any]]
    ): Value[Any] =
      new Value[Any] {
        override def caseValue: ValueCase[Value[Any]] = caseValue0
        override def annotations: ZEnvironment[Any]   = ZEnvironment.empty
      }

    def apply[Annotations](
        caseValue0: ValueCase[Value[Annotations]],
        annotations0: ZEnvironment[Annotations]
    ): Value[Annotations] =
      new Value[Annotations] {
        override def caseValue: ValueCase[Value[Annotations]] = caseValue0
        override def annotations: ZEnvironment[Annotations]   = annotations0
      }

    def field(name: Name)(record: Record[Any]): Field[Any] = Field(record, name, ZEnvironment.empty)
    def patternMatch(scrutinee: Value[Any], cases: (Value[Any], Value[Any])*): PatternMatch[Any] =
      PatternMatch(scrutinee, Chunk.fromIterable(cases), ZEnvironment.empty)
    // def patternMatch[Annotations](
    //     scrutinee: Value[Annotations],
    //     annotations: ZEnvironment[Annotations],
    //     cases: (Value[Annotations], Value[Annotations])*
    // ): PatternMatch[Annotations] =
    //   PatternMatch(scrutinee, Chunk.fromIterable(cases), annotations)

    def record(fields: (Name, Value[Any])*): Record[Any] =
      Record(Chunk.fromIterable(fields), ZEnvironment.empty)

    def wholeNumber(value: java.math.BigInteger): Literal[Any, java.math.BigInteger] = {
      println("In Value.wholeNumber")
      Literal(Lit.wholeNumber(value), ZEnvironment.empty)
    }

    def string(value: String): Literal[Any, String] = {
      println(s"In Value.string: $value")
      val l = Literal(Lit.string(value), ZEnvironment.empty)
      println(s"In Value.string: Literal: $l")
      l
    }

    import ValueCase.*

    final case class Field[+Annotations](
        target: Value[Annotations],
        name: Name,
        annotations: ZEnvironment[Annotations]
    ) extends Value[Annotations] {
      override lazy val caseValue: ValueCase[Value[Annotations]] = FieldCase(target, name)
    }

    final case class PatternMatch[+Annotations](
        scrutinee: Value[Annotations],
        cases: Chunk[(Value[Annotations], Value[Annotations])],
        annotations: ZEnvironment[Annotations]
    ) extends Value[Annotations] {
      override lazy val caseValue: ValueCase[Value[Annotations]] = PatternMatchCase(scrutinee, cases)
    }

    final case class Variable[+Annotations](name: Name, annotations: ZEnvironment[Annotations])
        extends Value[Annotations] {
      override lazy val caseValue: VariableCase = VariableCase(name)
    }

    final case class Record[+Annotations](
        fields: Chunk[(Name, Value[Annotations])],
        annotations: ZEnvironment[Annotations]
    ) extends Value[Annotations] {
      override lazy val caseValue: ValueCase[Value[Annotations]] = RecordCase(fields)
    }

    final case class Literal[+Annotations, +V](
        value: zio.morphir.ir.Literal[V],
        annotations: ZEnvironment[Annotations]
    ) extends Value[Annotations] {
      override lazy val caseValue: ValueCase[Value[Annotations]] = LiteralCase(value)
    }
  }

  sealed trait Distribution[+Annotations] extends MorphirIR[Annotations] { self =>
    def caseValue: DistributionCase[MorphirIR[Annotations]]
  }

  object Distribution {
    import DistributionCase.*
    final case class Library[+Annotations](
        packageName: PackageName,
        packageSpecs: Map[PackageName, PackageSpecification[Annotations]],
        packageDef: PackageDefinition[Annotations],
        annotations: ZEnvironment[Annotations]
    ) extends Distribution[Annotations] {
      override def caseValue: LibraryCase[MorphirIR[Annotations]] = LibraryCase(packageName, packageSpecs, packageDef)
    }
  }

  final case class ModuleDefinition[+Annotations](
      types: Map[Name, AccessControlled[Documented[TypeTree.Definition[Annotations]]]],
      values: Map[Name, AccessControlled[ValueTree.Definition[Annotations]]],
      annotations: ZEnvironment[Annotations]
  ) extends MorphirIR[Annotations] {
    override def caseValue: ModuleDefinitionCase[MorphirIR[Annotations]] = ModuleDefinitionCase(types, values)
  }

  object ModuleDefinition {
    val empty: ModuleDefinition[Any] = ModuleDefinition(Map.empty, Map.empty, ZEnvironment.empty)
  }

  final case class ModuleSpecification[+Annotations](
      types: Map[Name, Documented[TypeTree.Specification[Annotations]]],
      values: Map[Name, ValueTree.Specification[Annotations]],
      annotations: ZEnvironment[Annotations]
  ) extends MorphirIR[Annotations] {
    override def caseValue: ModuleSpecificationCase[MorphirIR[Annotations]] = ModuleSpecificationCase(types, values)
  }

  object ModuleSpecification {
    val empty: ModuleSpecification[Any] = ModuleSpecification(Map.empty, Map.empty, ZEnvironment.empty)
  }

  final case class PackageSpecification[+Annotations](
      modules: scala.collection.immutable.Map[ModuleName, ModuleSpecification[Annotations]],
      annotations: ZEnvironment[Annotations]
  ) extends MorphirIR[Annotations] {
    override def caseValue: PackageSpecificationCase[ModuleSpecification[Annotations]] = PackageSpecificationCase(
      modules
    )
  }

  final case class PackageDefinition[+Annotations](
      modules: Map[ModuleName, AccessControlled[ModuleDefinition[Annotations]]],
      annotations: ZEnvironment[Annotations]
  ) extends MorphirIR[Annotations] {
    override def caseValue: PackageDefinitionCase[ModuleDefinition[Annotations]] = PackageDefinitionCase(modules)
  }
}
