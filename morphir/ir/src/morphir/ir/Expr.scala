package morphir.ir

import morphir.ir.codec.{ typeCodecs, valueCodecs }

sealed abstract class Expr[+A](val tag: String) extends Product with Serializable {
  def isTypeExpr: Boolean
  def isValueExpr: Boolean = !isTypeExpr
  def attributes: A
}

sealed abstract class Type[+A](tag: String) extends Expr[A](tag) {
  final def isTypeExpr: Boolean = true
  def mapAttributes[B](f: A => B): Type[B]
}

sealed abstract class Value[+A](tag: String) extends Expr[A](tag) {
  final def isTypeExpr: Boolean = false
  def mapAttributes[B](f: A => B): Value[B]
}

object Type extends typeCodecs.TypeCodec {

  def record[A](fieldTypes: Field[A]*)(attributes: A): Record[A] = Record(attributes, fieldTypes.toList)
  def record(fieldTypes: Field[scala.Unit]*): Record[scala.Unit] = Record((), fieldTypes.toList)
  def field[A](name: Name, fieldType: Type[A]): Field[A]         = Field(name, fieldType)

  final case class Variable[+A](attributes: A, name: Name) extends Type[A](Variable.Tag) {
    override def mapAttributes[B](f: A => B): Type[B] = Variable[B](f(attributes), name)
  }

  object Variable extends typeCodecs.VariableCodec

  final case class Reference[+A](attributes: A, typeName: FQName, typeParameters: List[Type[A]])
      extends Type[A](Reference.Tag) {
    def this(attributes: A, typeName: FQName) =
      this(attributes, typeName, List.empty)
    def mapAttributes[B](f: A => B): Type[B] =
      Reference(f(attributes), typeName, typeParameters.map(t => t.mapAttributes(f)))
  }

  object Reference extends typeCodecs.ReferenceCodec {
    def apply[A](attributes: A, typeName: FQName): Reference[A] = new Reference(attributes, typeName)
  }

  final case class Tuple[+A](attributes: A, elementTypes: List[Type[A]]) extends Type[A](Tuple.Tag) {
    def mapAttributes[B](f: A => B): Type[B] = Tuple(f(attributes), elementTypes.map(e => e.mapAttributes(f)))
  }

  object Tuple extends typeCodecs.TupleCodec

  final case class Record[+A](attributes: A, fieldTypes: List[Field[A]]) extends Type[A](Record.Tag) {
    def mapAttributes[B](f: A => B): Type[B] = Record(f(attributes), fieldTypes.map(field => field.mapAttributes(f)))
  }

  object Record extends typeCodecs.RecordCodec

  final case class ExtensibleRecord[+A](attributes: A, variableName: Name, fieldTypes: List[Field[A]])
      extends Type[A](ExtensibleRecord.Tag) {
    def mapAttributes[B](f: A => B): Type[B] =
      ExtensibleRecord(f(attributes), variableName, fieldTypes.map(field => field.mapAttributes(f)))
  }

  object ExtensibleRecord extends typeCodecs.ExtensibleRecordCodec

  final case class Function[+A](attributes: A, argumentType: Type[A], returnType: Type[A])
      extends Type[A](Function.Tag) {
    def mapAttributes[B](f: A => B): Type[B] =
      Function(f(attributes), argumentType.mapAttributes(f), returnType.mapAttributes(f))
  }

  object Function extends typeCodecs.FunctionCodec

  final case class Unit[+A](attributes: A) extends Type[A](Unit.Tag) {
    def mapAttributes[B](f: A => B): Type[B] = Unit(f(attributes))
  }

  object Unit extends typeCodecs.UnitCodec

  sealed abstract class Specification[+A] extends Product with Serializable {
    def mapAttributes[B](f: A => B): Specification[B]
  }
  object Specification {

    final case class TypeAliasSpecification[+A](typeParams: List[Name], typeExp: Type[A]) extends Specification[A] {
      def mapAttributes[B](f: A => B): Specification[B] =
        TypeAliasSpecification(typeParams, typeExp.mapAttributes(f))
    }

    object TypeAliasSpecification {
      implicit def nameTag[A]: NameTag[TypeAliasSpecification[A]] = NameTag.forClass[TypeAliasSpecification[A]]
    }

    final case class OpaqueTypeSpecification(typeParams: List[Name]) extends Specification[Nothing] {
      def mapAttributes[B](f: Nothing => B): Specification[B] = this
    }

    object OpaqueTypeSpecification {
      implicit val nameTag: NameTag[OpaqueTypeSpecification] = NameTag.forClass[OpaqueTypeSpecification]
    }

    final case class CustomTypeSpecification[+A](typeParams: List[Name], constructors: Constructors[A])
        extends Specification[A] {
      def mapAttributes[B](f: A => B): Specification[B] =
        CustomTypeSpecification(typeParams, constructors.mapAttributes(f))
    }

    object CustomTypeSpecification {
      implicit def nameTag[A]: NameTag[CustomTypeSpecification[A]] = NameTag.forClass[CustomTypeSpecification[A]]
    }
  }

  sealed abstract class Definition[+A] extends Product with Serializable {
    def toSpecification: Specification[A]
  }
  object Definition extends typeCodecs.DefinitionCodec {

    final case class TypeAliasDefinition[+A](typeParams: scala.List[Name], typeExp: Type[A]) extends Definition[A] {
      def toSpecification: Specification[A] = Specification.TypeAliasSpecification(typeParams, typeExp)
    }

    object TypeAliasDefinition extends typeCodecs.TypeAliasDefinitionCodec

    final case class CustomTypeDefinition[+A](typeParams: List[Name], ctors: AccessControlled[Constructors[A]])
        extends Definition[A] {
      def toSpecification: Specification[A] =
        ctors.fold(
          constructors => Specification.CustomTypeSpecification(typeParams, constructors),
          _ => Specification.OpaqueTypeSpecification(typeParams)
        )
    }

    object CustomTypeDefinition extends typeCodecs.CustomTypeDefinitionCodec
  }

  final case class Constructors[+A](toList: List[Constructor[A]]) extends AnyVal {
    def mapAttributes[B](f: A => B): Constructors[B] =
      Constructors(toList.map(constructor => constructor.mapAttributes(f)))
  }

  object Constructors extends typeCodecs.ConstructorsCodec

  final case class Constructor[+A](name: Name, args: List[(Name, Type[A])]) {
    def mapAttributes[B](f: A => B): Constructor[B] =
      Constructor(name, args.map { case (name, argType) => name -> argType.mapAttributes(f) })
  }

  object Constructor extends typeCodecs.ConstructorCodec {

    implicit def nameTag[A]: NameTag[Constructor[A]] = NameTag.fromString("constructor")
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

  object Field extends typeCodecs.FieldCodec
}

object Value extends valueCodecs.ValueCodec {

  type ValueExprList[+A] = scala.List[Value[A]]

  import pattern.Pattern

  def literal[A](attributes: A, value: morphir.ir.literal.Literal): Literal[A] = Literal(attributes, value)
  def literal[A](attributes: A, value: Boolean): Literal[A]                    = Literal(attributes, morphir.ir.literal.bool(value))
  def unit[A](attributes: A): Unit[A]                                          = Unit(attributes)

  final case class Literal[+A](attributes: A, value: morphir.ir.literal.Literal) extends Value[A](Literal.Tag) {
    def mapAttributes[B](f: A => B): Value[B] = Literal(f(attributes), value)
  }

  object Literal extends valueCodecs.LiteralCodec

  final case class Constructor[+A](attributes: A, fullyQualifiedName: FQName) extends Value[A](Constructor.Tag) {
    def mapAttributes[B](f: A => B): Value[B] = Constructor(f(attributes), fullyQualifiedName)
  }
  object Constructor extends valueCodecs.ConstructorCodec

  final case class Tuple[+A](attributes: A, elements: scala.List[Value[A]]) extends Value[A](Tuple.Tag) {
    def mapAttributes[B](f: A => B): Value[B] = Tuple(f(attributes), elements.mapAttributes(f))
  }

  object Tuple extends valueCodecs.TupleCodec

  final case class List[+A](attributes: A, items: scala.List[Value[A]]) extends Value[A](List.Tag) {
    def mapAttributes[B](f: A => B): Value[B] = List(f(attributes), items.mapAttributes(f))
  }

  object List extends valueCodecs.ListCodec

  final case class Record[+A](attributes: A, fields: scala.List[(Name, Value[A])]) extends Value[A](Record.Tag) {
    def mapAttributes[B](f: A => B): Value[B] =
      Record(f(attributes), fields.map {
        case (name, valueExpr) => (name, valueExpr.mapAttributes(f))
      })
  }

  object Record extends valueCodecs.RecordCodec

  final case class Variable[+A](attributes: A, name: Name) extends Value[A](Variable.Tag) {
    def mapAttributes[B](f: A => B): Value[B] = Variable(f(attributes), name)
  }

  object Variable extends valueCodecs.VariableCodec

  final case class Reference[+A](attributes: A, fullyQualifiedName: FQName) extends Value[A](Reference.Tag) {
    def mapAttributes[B](f: A => B): Value[B] = Reference(f(attributes), fullyQualifiedName)
  }

  object Reference extends valueCodecs.ReferenceCodec

  final case class Field[+A](attributes: A, subjectValue: Value[A], fieldName: Name) extends Value[A](Field.Tag) {
    def mapAttributes[B](f: A => B): Value[B] = Field(f(attributes), subjectValue.mapAttributes(f), fieldName)
  }

  object Field extends valueCodecs.FieldCodec

  final case class FieldFunction[+A](attributes: A, fieldName: Name) extends Value[A](FieldFunction.Tag) {
    def mapAttributes[B](f: A => B): Value[B] = FieldFunction(f(attributes), fieldName)
  }

  object FieldFunction extends valueCodecs.FieldFunctionCodec

  final case class Apply[+A](attributes: A, function: Value[A], argument: Value[A]) extends Value[A](Apply.Tag) {
    def mapAttributes[B](f: A => B): Value[B] =
      Apply(f(attributes), function.mapAttributes(f), argument.mapAttributes(f))
  }

  object Apply extends valueCodecs.ApplyCodec

  final case class Lambda[+A](attributes: A, argumentPattern: pattern.Pattern[A], body: Value[A])
      extends Value[A](Lambda.Tag) {
    def mapAttributes[B](f: A => B): Value[B] =
      Lambda(f(attributes), argumentPattern.mapAttributes(f), body.mapAttributes(f))
  }

  object Lambda extends valueCodecs.LambdaCodec

  final case class LetDefinition[+A](attributes: A, valueName: Name, valueDefinition: Definition[A], inValue: Value[A])
      extends Value[A](LetDefinition.Tag) {
    def mapAttributes[B](f: A => B): Value[B] =
      LetDefinition(f(attributes), valueName, valueDefinition.mapAttributes(f), inValue.mapAttributes(f))
  }

  object LetDefinition extends valueCodecs.LetDefinitionCodec

  final case class LetRecursion[+A](attributes: A, valueDefinitions: Map[Name, Definition[A]], inValue: Value[A])
      extends Value[A](LetRecursion.Tag) {
    def mapAttributes[B](f: A => B): Value[B] =
      LetRecursion(f(attributes), valueDefinitions.map {
        case (name, definition) => name -> definition.mapAttributes(f)
      }, inValue.mapAttributes(f))
  }

  object LetRecursion extends valueCodecs.LetRecursionCodec

  final case class Destructure[+A](
    attributes: A,
    pattern: Pattern[A],
    valueToDestruct: Value[A],
    inValue: Value[A]
  ) extends Value[A](Destructure.Tag) {
    def mapAttributes[B](f: A => B): Value[B] =
      Destructure(f(attributes), pattern.mapAttributes(f), valueToDestruct.mapAttributes(f), inValue.mapAttributes(f))
  }

  object Destructure extends valueCodecs.DestructureCodec

  final case class IfThenElse[+A](attributes: A, condition: Value[A], thenBranch: Value[A], elseBranch: Value[A])
      extends Value[A](IfThenElse.Tag) {
    def mapAttributes[B](f: A => B): Value[B] =
      IfThenElse(f(attributes), condition.mapAttributes(f), thenBranch.mapAttributes(f), elseBranch.mapAttributes(f))
  }

  object IfThenElse extends valueCodecs.IfThenElseCodec

  final case class PatternMatch[+A](attributes: A, branchOutOn: Value[A], cases: scala.List[(Pattern[A], Value[A])])
      extends Value[A](PatternMatch.Tag) {
    def mapAttributes[B](f: A => B): Value[B] =
      PatternMatch(f(attributes), branchOutOn.mapAttributes(f), cases.map {
        case (pat, valueExpr) => (pat.mapAttributes(f), valueExpr.mapAttributes(f))
      })
  }

  object PatternMatch extends valueCodecs.PatternMatchCodec

  final case class UpdateRecord[+A](
    attributes: A,
    valueToUpdate: Value[A],
    fieldsToUpdate: scala.List[(Name, Value[A])]
  ) extends Value[A](UpdateRecord.Tag) {

    def mapAttributes[B](f: A => B): Value[B] =
      UpdateRecord(f(attributes), valueToUpdate.mapAttributes(f), fieldsToUpdate.map {
        case (name, valueExpr) => (name, valueExpr.mapAttributes(f))
      })
  }

  object UpdateRecord extends valueCodecs.UpdateRecordCodec

  final case class Unit[+A](attributes: A) extends Value[A](Unit.Tag) {
    def mapAttributes[B](f: A => B): Value[B] = Unit(f(attributes))
  }

  object Unit extends valueCodecs.UnitCodec

  final case class Specification[+A](inputs: scala.List[(Name, Type[A])], output: Type[A]) {
    def mapAttributes[B](f: A => B): Specification[B] =
      Specification(inputs.map {
        case (name, tpe) => (name, tpe.mapAttributes(f))
      }, output.mapAttributes(f))
  }

  object Specification

  final case class Definition[+A](
    valueType: Option[Type[A]],
    arguments: argument.ArgumentList[A],
    body: Value[A]
  ) {
    def mapAttributes[B](f: A => B): Definition[B] =
      Definition(valueType.map(_.mapAttributes(f)), arguments.mapValue(f), body.mapAttributes(f))
  }

  object Definition extends valueCodecs.DefinitionCodec

  implicit class ValueExprListOps[+A](private val self: ValueExprList[A]) extends AnyVal {
    def mapAttributes[B](f: A => B): ValueExprList[B] =
      self.map(_.mapAttributes(f))
  }
}
