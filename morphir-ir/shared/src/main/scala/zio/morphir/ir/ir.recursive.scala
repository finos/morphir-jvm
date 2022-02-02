package zio.morphir.ir
import zio.Chunk
import zio.prelude.*
object recursive {

  sealed trait IRCase[+Self] { self =>

    def map[B](f: Self => B): IRCase[B] =
      self match {
        case c: TypeTreeCase[s]             => c.map(f)
        case c: ValueCase[s]                => c.map(f)
        case c: PatternCase[s]              => c.map(f)
        case c: PackageSpecificationCase[s] => c.map(f)

      }
  }
  object IRCase {
    type TypeCase[+Self] = zio.morphir.ir.recursive.TypeCase[Self]
    val TypeCase = zio.morphir.ir.recursive.TypeCase

    type ValueCase[+Self] = zio.morphir.ir.recursive.ValueCase[Self]
    val ValueCase = zio.morphir.ir.recursive.ValueCase

    type PackageSpecificationCase[+Self] = zio.morphir.ir.recursive.PackageSpecificationCase[Self]
    val PackageSpecificationCase = zio.morphir.ir.recursive.PackageSpecificationCase
  }

  final case class PackageSpecificationCase[+Self](modules: Map[Name, Self]) extends IRCase[Self]

  sealed trait TypeTreeCase[+Self] extends IRCase[Self] { self =>
    import TypeTreeCase.*
    import DefinitionCase.*
    import SpecificationCase.*

    override def map[B](f: Self => B): TypeTreeCase[B] =
      self match {
        case c @ ConstructorsCase(_) => ConstructorsCase(c.args.map { case (name, argType) => (name, f(argType)) })
        case c @ CustomTypeDefinitionCase(_, _)    => CustomTypeDefinitionCase(c.typeParams, c.ctors.map(f))
        case c @ CustomTypeSpecificationCase(_, _) => CustomTypeSpecificationCase(c.typeParams, f(c.ctors))
        case c @ OpaqueTypeSpecificationCase(_)    => c
        case c @ TypeAliasDefinitionCase(_, _)     => TypeAliasDefinitionCase(c.typeParams, f(c.typeExpr))
        case c @ TypeAliasSpecificationCase(_, _)  => TypeAliasSpecificationCase(c.typeParams, f(c.typeExpr))
        case c: TypeExprCase[s]                    => c.map(f)
      }
  }
  object TypeTreeCase {
    final case class ConstructorsCase[+Self](args: Map[Name, Self]) extends TypeTreeCase[Self]
    sealed trait StatementCase[+Self]                               extends TypeTreeCase[Self]

    sealed trait DefinitionCase[+Self] extends StatementCase[Self]
    object DefinitionCase {
      final case class CustomTypeDefinitionCase[+Self](typeParams: List[Name], ctors: AccessControlled[Self])
          extends DefinitionCase[Self]
      final case class TypeAliasDefinitionCase[+Self](typeParams: List[Name], typeExpr: Self)
          extends DefinitionCase[Self]
    }

    sealed trait SpecificationCase[+Self] extends StatementCase[Self] {
      def typeParams: List[Name]
    }

    object SpecificationCase {
      object TypeParams {
        def unapply[Self](irCase: IRCase[Self]): Option[List[Name]] = irCase match {
          case c: SpecificationCase[s] => Some(c.typeParams)
          case _                       => None
        }
      }
      final case class CustomTypeSpecificationCase[+Self](typeParams: List[Name], ctors: Self)
          extends SpecificationCase[Self]
      final case class OpaqueTypeSpecificationCase(typeParams: List[Name]) extends SpecificationCase[Nothing]
      final case class TypeAliasSpecificationCase[+Self](typeParams: List[Name], typeExpr: Self)
          extends SpecificationCase[Self]
    }

    type TypeExprCase[+Self] = zio.morphir.ir.recursive.TypeCase[Self]
    val TypeExprCase = zio.morphir.ir.recursive.TypeCase
  }

  sealed trait TypeCase[+Self] extends TypeTreeCase[Self] { self =>
    import TypeCase.*
    override def map[B](f: Self => B): TypeCase[B] = self match {
      case c @ ExtensibleRecordCase(_, _) => ExtensibleRecordCase(c.name, c.fields.map(f))
      case c @ FieldCase(_, _)            => FieldCase(c.name, f(c.fieldType))
      case c @ FunctionCase(_, _)         => FunctionCase(c.paramTypes.map(f), f(c.returnType))
      case c @ ReferenceCase(_, _)        => ReferenceCase(c.typeName, c.typeParams.map(f))
      case c @ TupleCase(_)               => TupleCase(c.elementTypes.map(f))
      case UnitCase                       => UnitCase
      case c @ VariableCase(_)            => VariableCase(c.name)
      case c @ RecordCase(_)              => RecordCase(c.fields.map(f))
    }
  }

  object TypeCase {
    final case class ExtensibleRecordCase[+Self](name: Name, fields: Chunk[Self])    extends TypeCase[Self]
    final case class FunctionCase[+Self](paramTypes: List[Self], returnType: Self)   extends TypeCase[Self]
    final case class RecordCase[+Self](fields: Chunk[Self])                          extends TypeCase[Self]
    final case class ReferenceCase[+Self](typeName: FQName, typeParams: Chunk[Self]) extends TypeCase[Self]
    final case class TupleCase[+Self](elementTypes: List[Self])                      extends TypeCase[Self]
    case object UnitCase                                                             extends TypeCase[Nothing]
    final case class VariableCase(name: Name)                                        extends TypeCase[Nothing]
    final case class FieldCase[+Self](name: Name, fieldType: Self)                   extends TypeCase[Self]

    implicit val TypeCaseCovariant: Covariant[TypeCase] = new Covariant[TypeCase] {
      def map[A, B](f: A => B): TypeCase[A] => TypeCase[B] = _.map(f)
    }
  }

  sealed trait ValueCase[+Self] extends IRCase[Self] { self =>
    import ValueCase.*
    override def map[B](f: Self => B): ValueCase[B] = self match {
      case c @ ApplyCase(_, _)      => ApplyCase(f(c.function), c.arguments.map(f))
      case c @ ConstructorCase(_)   => ConstructorCase(c.name)
      case c @ FieldCase(_, _)      => FieldCase(f(c.target), c.name)
      case c @ FieldFunctionCase(_) => FieldFunctionCase(c.name)
      case c @ IfThenElseCase(_, _, _) =>
        IfThenElseCase(f(c.condition), f(c.thenBranch), f(c.elseBranch))
      case c @ ListCase(_)    => ListCase(c.elements.map(f))
      case c @ LiteralCase(_) => LiteralCase(c.literal)
      case c @ PatternMatchCase(_, _) =>
        PatternMatchCase(f(c.branchOutOn), c.cases.map { case (p, v) => (f(p), f(v)) })
      case c @ RecordCase(_)    => RecordCase(c.fields.map { case (name, value) => (name, f(value)) })
      case c @ ReferenceCase(_) => c
      case c @ TupleCase(_)     => TupleCase(c.elements.map(f))
      case _ @UnitCase          => UnitCase
      case c @ VariableCase(_)  => c
    }
  }
  object ValueCase {
    final case class ApplyCase[+Self](function: Self, arguments: List[Self]) extends ValueCase[Self]
    final case class ConstructorCase(name: FQName)                           extends ValueCase[Nothing]
    final case class FieldCase[+Self](target: Self, name: Name)              extends ValueCase[Self]
    final case class FieldFunctionCase(name: Name)                           extends ValueCase[Nothing]
    final case class IfThenElseCase[+Self](condition: Self, thenBranch: Self, elseBranch: Self) extends ValueCase[Self]
    final case class ListCase[+Self](elements: List[Self])                                      extends ValueCase[Self]
    final case class LiteralCase(literal: LiteralValue)                                    extends ValueCase[Nothing]
    final case class PatternMatchCase[+Self](branchOutOn: Self, cases: List[(Self, Self)]) extends ValueCase[Self]
    final case class RecordCase[+Self](fields: List[(Name, Self)])                         extends ValueCase[Self]
    final case class ReferenceCase(name: FQName)                                           extends ValueCase[Nothing]
    final case class TupleCase[+Self](elements: List[Self])                                extends ValueCase[Self]
    case object UnitCase                                                                   extends ValueCase[Nothing]
    final case class VariableCase(name: Name)                                              extends ValueCase[Nothing]

    implicit val ValueCaseCovariant: Covariant[ValueCase] = new Covariant[ValueCase] {
      def map[A, B](f: A => B): ValueCase[A] => ValueCase[B] = _.map(f)
    }
  }

  sealed trait PatternCase[+Self] extends IRCase[Self] { self =>
    import PatternCase.*

    override def map[B](f: Self => B): PatternCase[B] = self match {
      case c @ AsCase(_, _)          => AsCase(f(c.pattern), c.name)
      case c @ ConstructorCase(_, _) => ConstructorCase(c.constructorName, c.argumentPatterns.map(f))
      case EmptyListCase             => EmptyListCase
      case c @ HeadTailCase(_, _)    => HeadTailCase(f(c.head), f(c.tail))
      case c @ LiteralCase(_)        => c
      case c @ TupleCase(_)          => TupleCase(c.elements.map(f))
      case UnitCase                  => UnitCase
      case WildcardCase              => WildcardCase
    }

  }

  object PatternCase {
    final case class AsCase[+Self](pattern: Self, name: Name) extends PatternCase[Self]
    final case class ConstructorCase[+Self](constructorName: FQName, argumentPatterns: List[Self])
        extends PatternCase[Self]
    case object EmptyListCase                                    extends PatternCase[Nothing]
    final case class HeadTailCase[+Self](head: Self, tail: Self) extends PatternCase[Self]
    final case class LiteralCase(value: Literal[Nothing])        extends PatternCase[Nothing]
    final case class TupleCase[+Self](elements: List[Self])      extends PatternCase[Self]
    case object UnitCase                                         extends PatternCase[Nothing]
    case object WildcardCase                                     extends PatternCase[Nothing]
  }
}
