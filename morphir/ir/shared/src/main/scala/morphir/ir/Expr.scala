package morphir.ir

import morphir.ir.codec.{ `type` => typeCodec }

sealed abstract class Expr[+K <: ExprKind, +A](val kind: K) extends Product with Serializable {
  final def tag: String = kind.tag
  def attributes: A
}

sealed abstract class Type[+A](kind: TypeExprKind) extends Expr[TypeExprKind, A](kind) {
  final def isTypeExpr: Boolean = true
  def mapAttributes[B](f: A => B): Type[B]
}

object Type extends typeCodec.TypeCoproductCodec {

  final case class Variable[+A](attributes: A, name: Name) extends Type[A](TypeExprKind.Variable) {
    override def mapAttributes[B](f: A => B): Type[B] = Variable[B](f(attributes), name)
  }
  object Variable extends typeCodec.VariableCodec

  final case class Reference[+A](attributes: A, typeName: FQName, typeParameters: List[Type[A]])
      extends Type[A](TypeExprKind.Reference) {
    def mapAttributes[B](f: A => B): Type[B] =
      Reference(f(attributes), typeName, typeParameters.map(t => t.mapAttributes(f)))
  }

  object Reference extends typeCodec.ReferenceCodec

  final case class Tuple[+A](attributes: A, elementTypes: List[Type[A]]) extends Type[A](TypeExprKind.Tuple) {
    def mapAttributes[B](f: A => B): Type[B] = Tuple(f(attributes), elementTypes.map(e => e.mapAttributes(f)))
  }
  object Tuple extends typeCodec.TupleCodec

  final case class Record[+A](attributes: A, fieldTypes: List[Field[A]]) extends Type[A](TypeExprKind.Record) {
    def mapAttributes[B](f: A => B): Type[B] = Record(f(attributes), fieldTypes.map(field => field.mapAttributes(f)))
  }
  object Record extends typeCodec.RecordCodec

  final case class ExtensibleRecord[+A](attributes: A, variableName: Name, fieldTypes: List[Field[A]])
      extends Type[A](TypeExprKind.ExtensibleRecord) {
    def mapAttributes[B](f: A => B): Type[B] =
      ExtensibleRecord(f(attributes), variableName, fieldTypes.map(field => field.mapAttributes(f)))
  }
  object ExtensibleRecord extends typeCodec.ExtensibleRecordCodec

  final case class Function[+A](attributes: A, argumentType: Type[A], returnType: Type[A])
      extends Type[A](TypeExprKind.Function) {
    def mapAttributes[B](f: A => B): Type[B] =
      Function(f(attributes), argumentType.mapAttributes(f), returnType.mapAttributes(f))
  }
  object Function extends typeCodec.FunctionCodec

  final case class Unit[+A](attributes: A) extends Type[A](TypeExprKind.Unit) {
    def mapAttributes[B](f: A => B): Type[B] = Unit(f(attributes))
  }
  object Unit extends typeCodec.UnitCodec

  sealed abstract class Specification[+A] extends Product with Serializable {}
  object Specification {
    final case class TypeAliasSpecification[+A](typeParams: List[Name], typeExp: Type[A]) extends Specification[A]
    final case class OpaqueTypeSpecification(typeParams: List[Name])                      extends Specification[Nothing]
    final case class CustomTypeSpecification[+A](typeParams: List[Name], constructors: Constructors[A])
        extends Specification[A]

  }

  sealed abstract class Definition[+A] extends Product with Serializable {
    def toSpecification: Specification[A]
  }
  object Definition {

    final case class TypeAliasDefinition[+A](typeParams: List[Name], typeExp: Type[A]) extends Definition[A] {
      def toSpecification: Specification[A] = Specification.TypeAliasSpecification(typeParams, typeExp)
    }

    final case class CustomTypeDefinition[+A](typeParams: List[Name], ctors: AccessControlled[Constructors[A]])
        extends Definition[A] {
      def toSpecification: Specification[A] =
        ctors.fold(
          constructors => Specification.CustomTypeSpecification(typeParams, constructors),
          _ => Specification.OpaqueTypeSpecification(typeParams)
        )
    }
  }

  final case class Constructors[+A](constructors: List[Constructor[A]]) extends AnyVal {
    def mapAttributes[B](f: A => B): Constructors[B] =
      Constructors(constructors.map(constructor => constructor.mapAttributes(f)))
  }

  final case class Constructor[+A](name: Name, args: List[(Name, Type[A])]) {
    def mapAttributes[B](f: A => B): Constructor[B] =
      Constructor(name, args.map { case (name, argType) => name -> argType.mapAttributes(f) })
  }

  final case class Field[+A](name: Name, fieldType: Type[A]) {
    def map[B](f: (Name, Type[A]) => (Name, Type[B])): Field[B] = {
      val (newName, newType) = f(name, fieldType)
      Field(newName, newType)
    }
    def mapFieldName(f: Name => Name): Field[A]          = copy(name = f(name))
    def mapFieldType[B](f: Type[A] => Type[B]): Field[B] = copy(fieldType = f(fieldType))
    def mapAttributes[B](f: A => B): Field[B]            = Field(name, fieldType.mapAttributes(f))
  }
  object Field extends typeCodec.FieldCodec
}

sealed abstract class Value[+A](kind: ValueExprKind) extends Expr[ValueExprKind, A](kind) {
  final def isTypeExpr: Boolean = false
}

object Value {

  final case class Literal[+A, +L](attributes: A, value: LiteralValue[L]) extends Value[A](ValueExprKind.Literal)
  final case class Constructor[+A](attributes: A, fullyQualifiedName: FQName)
      extends Value[A](ValueExprKind.Constructor)
  final case class Tuple[+A](attributes: A, elements: List[Value[A]])       extends Value[A](ValueExprKind.Tuple)
  final case class List[+A](attributes: A, items: List[Value[A]])           extends Value[A](ValueExprKind.List)
  final case class Record[+A](attributes: A, fields: RecordFields[A])       extends Value[A](ValueExprKind.Record)
  final case class Variable[+A](attributes: A, name: Name)                  extends Value[A](ValueExprKind.Variable)
  final case class Reference[+A](attributes: A, fullyQualifiedName: FQName) extends Value[A](ValueExprKind.Reference)
  final case class Field[+A](attributes: A, subjectValue: Value[A], fieldName: Name)
      extends Value[A](ValueExprKind.Field)
  final case class FieldFunction[+A](attributes: A, fieldName: Name) extends Value[A](ValueExprKind.FieldFunction)
  final case class Apply[+A](attributes: A, function: Value[A], argument: Value[A])
      extends Value[A](ValueExprKind.Apply)
  final case class Lambda[+A](attributes: A, argumentPattern: Pattern[A], body: Value[A])
      extends Value[A](ValueExprKind.Lambda)
  final case class LetDefinition[+A](attributes: A, valueName: Name, valueDefinition: Definition[A], inValue: Value[A])
      extends Value[A](ValueExprKind.LetDefinition)
  final case class LetRecursion[+A](attributes: A, valueDefinitions: Map[Name, Definition[A]], inValue: Value[A])
      extends Value[A](ValueExprKind.LetRecursion)
  final case class Destructure[+A](attributes: A, pattern: Pattern[A], valueToDestruct: Value[A], inValue: Value[A])
      extends Value[A](ValueExprKind.Destructure)
  final case class IfThenElse[+A](attributes: A, condition: Value[A], thenBranch: Value[A], elseBranch: Value[A])
      extends Value[A](ValueExprKind.IfThenElse)
  final case class PatternMatch[A](attributes: A, branchOutOn: Value[A], cases: PatternMatchCases[A])
      extends Value[A](ValueExprKind.PatternMatch)
  final case class UpdateRecord[+A](attributes: A, valueToUpdate: Value[A], fieldsToUpdate: RecordFields[A])
      extends Value[A](ValueExprKind.UpdateRecord)
  final case class Unit[+A](attributes: A) extends Value[A](ValueExprKind.Unit)

  sealed abstract class Pattern[+A] extends Product with Serializable {
    def attributes: A
  }

  object Pattern {
    final case class WildcardPattern[+A](attributes: A)                                 extends Pattern[A]
    final case class AsPattern[+A](attributes: A, pattern: Pattern[A], name: Name)      extends Pattern[A]
    final case class TuplePattern[+A](attributes: A, elementPatterns: List[Pattern[A]]) extends Pattern[A]
    final case class RecordPattern[+A](attributes: A, fieldNames: List[Name])           extends Pattern[A]
    final case class ConstructorPattern[+A](attributes: A, constructorName: FQName, argumentPatterns: List[Pattern[A]])
        extends Pattern[A]
    final case class EmptyListPattern[+A](attributes: A) extends Pattern[A]
    final case class HeadTailPattern[+A](attributes: A, headPattern: Pattern[A], tailPattern: Pattern[A])
        extends Pattern[A]
    final case class LiteralPattern[+A, +L](attributes: A, value: LiteralValue[L]) extends Pattern[A]
    final case class UnitPattern[+A](attributes: A)                                extends Pattern[A]
  }

  final case class Specification[+A](inputs: ParameterList[A], output: Type[A])
  final case class Definition[+A](valueType: Option[Type[A]], arguments: ArgumentList[A], body: Value[A])
}
