package morphir.ir

import cats.syntax.functor._
import io.circe.{ Decoder, Encoder }
import io.circe.syntax._

sealed abstract class Expr[+K <: ExprKind, +A](val kind: K) extends Product with Serializable {
  final def tag: String = kind.tag
  def attributes: A
}

sealed abstract class Type[+A](kind: TypeExprKind) extends Expr[TypeExprKind, A](kind) {
  final def isTypeExpr: Boolean = true
  def mapAttributes[B](f: A => B): Type[B]
}

sealed abstract class Value[+A](kind: ValueExprKind) extends Expr[ValueExprKind, A](kind) {
  final def isTypeExpr: Boolean = false
  def mapAttributes[B](f: A => B): Value[B]
}

sealed abstract class ExprCompanion(val Tag: String) {
  def hasMatchingTag[P <: Product](product: P): Boolean =
    if (product.productArity < 1) false
    else product.productElement(0) == Tag
}

object Type {

  def record[A](fieldTypes: Field[A]*)(attributes: A): Record[A] = Record(attributes, fieldTypes.toList)
  def record(fieldTypes: Field[scala.Unit]*): Record[scala.Unit] = Record((), fieldTypes.toList)
  def field[A](name: Name, fieldType: Type[A]): Field[A]         = Field(name, fieldType)

  final case class Variable[+A](attributes: A, name: Name) extends Type[A](TypeExprKind.Variable) {
    override def mapAttributes[B](f: A => B): Type[B] = Variable[B](f(attributes), name)
  }

  final case class Reference[+A](attributes: A, typeName: FQName, typeParameters: List[Type[A]])
      extends Type[A](TypeExprKind.Reference) {
    def this(attributes: A, typeName: FQName) =
      this(attributes, typeName, List.empty)
    def mapAttributes[B](f: A => B): Type[B] =
      Reference(f(attributes), typeName, typeParameters.map(t => t.mapAttributes(f)))
  }

  final case class Tuple[+A](attributes: A, elementTypes: List[Type[A]]) extends Type[A](TypeExprKind.Tuple) {
    def mapAttributes[B](f: A => B): Type[B] = Tuple(f(attributes), elementTypes.map(e => e.mapAttributes(f)))
  }

  final case class Record[+A](attributes: A, fieldTypes: List[Field[A]]) extends Type[A](TypeExprKind.Record) {
    def mapAttributes[B](f: A => B): Type[B] = Record(f(attributes), fieldTypes.map(field => field.mapAttributes(f)))
  }

  final case class ExtensibleRecord[+A](attributes: A, variableName: Name, fieldTypes: List[Field[A]])
      extends Type[A](TypeExprKind.ExtensibleRecord) {
    def mapAttributes[B](f: A => B): Type[B] =
      ExtensibleRecord(f(attributes), variableName, fieldTypes.map(field => field.mapAttributes(f)))
  }

  final case class Function[+A](attributes: A, argumentType: Type[A], returnType: Type[A])
      extends Type[A](TypeExprKind.Function) {
    def mapAttributes[B](f: A => B): Type[B] =
      Function(f(attributes), argumentType.mapAttributes(f), returnType.mapAttributes(f))
  }

  final case class Unit[+A](attributes: A) extends Type[A](TypeExprKind.Unit) {
    def mapAttributes[B](f: A => B): Type[B] = Unit(f(attributes))
  }

  object Variable extends ExprCompanion("variable") {
    implicit def encodeVariable[A: Encoder]: Encoder[Variable[A]] =
      Encoder.encodeTuple3[String, A, Name].contramap(exp => (Tag, exp.attributes, exp.name))

    implicit def decodeVariable[A: Decoder]: Decoder[Variable[A]] =
      Decoder
        .decodeTuple3[String, A, Name]
        .ensure(
          pred = hasMatchingTag,
          message = s"""The tag of a type variable must be "$Tag"."""
        )
        .map {
          case (_, attributes, name) => Variable(attributes, name)
        }
  }

  object Reference extends ExprCompanion("reference") {
    implicit def encodeReferenceType[A: Encoder](implicit typeEncoder: Encoder[Type[A]]): Encoder[Reference[A]] =
      Encoder
        .encodeTuple4[String, A, FQName, List[Type[A]]]
        .contramap(x => (Tag, x.attributes, x.typeName, x.typeParameters))

    implicit def decodeReferenceType[A: Decoder](implicit typeDecoder: Decoder[Type[A]]): Decoder[Reference[A]] =
      Decoder
        .decodeTuple4[String, A, FQName, List[Type[A]]]
        .ensure(
          pred = hasMatchingTag,
          message = s"""The tag of a type reference must be "$Tag"."""
        )
        .map {
          case (_, attributes, typeName, typeParameters) => Reference(attributes, typeName, typeParameters)
        }

    def apply[A](attributes: A, typeName: FQName): Reference[A] = new Reference(attributes, typeName)
  }

  object Tuple extends ExprCompanion("tuple") {
    implicit def encodeTupleType[A: Encoder]: Encoder[Tuple[A]] =
      Encoder
        .encodeTuple3[String, A, List[Type[A]]]
        .contramap(tuple => (Tag, tuple.attributes, tuple.elementTypes))

    implicit def decodeTupleType[A: Decoder]: Decoder[Type.Tuple[A]] =
      Decoder
        .decodeTuple3[String, A, List[Type[A]]]
        .ensure(hasMatchingTag, s"""The tag of a tuple type must be "$Tag".""")
        .map {
          case (_, attributes, elements) => Type.Tuple(attributes, elements)
        }
  }
  object Record extends ExprCompanion("record") {
    implicit def encodeRecordType[A: Encoder]: Encoder[Type.Record[A]] =
      Encoder.encodeTuple3[String, A, List[Field[A]]].contramap(rec => (Tag, rec.attributes, rec.fieldTypes))

    implicit def decodeRecordType[A: Decoder]: Decoder[Type.Record[A]] =
      Decoder
        .decodeTuple3[String, A, List[Field[A]]]
        .ensure(hasMatchingTag, s"""The tag of a record type must be "$Tag".""")
        .map { case (_, attributes, fields) => Record(attributes, fields) }
  }

  object ExtensibleRecord extends ExprCompanion("extensible_record") {

    implicit def encodeExtensibleRecordType[A: Encoder]: Encoder[Type.ExtensibleRecord[A]] =
      Encoder
        .encodeTuple4[String, A, Name, List[Field[A]]]
        .contramap(rec => (Tag, rec.attributes, rec.variableName, rec.fieldTypes))

    implicit def decodeExtensibleRecordType[A: Decoder]: Decoder[Type.ExtensibleRecord[A]] =
      Decoder
        .decodeTuple4[String, A, Name, List[Field[A]]]
        .ensure(hasMatchingTag, s"""The tag of an extensible record type must be "$Tag".""")
        .map {
          case (_, attributes, name, fields) => ExtensibleRecord(attributes, name, fields)
        }
  }

  object Function extends ExprCompanion("function") {
    implicit def encodeFunctionType[A: Encoder]: Encoder[Type.Function[A]] =
      Encoder
        .encodeTuple4[String, A, Type[A], Type[A]]
        .contramap(ft => (ft.tag, ft.attributes, ft.argumentType, ft.returnType))

    implicit def decodeFunctionType[A: Decoder]: Decoder[Type.Function[A]] =
      Decoder
        .decodeTuple4[String, A, Type[A], Type[A]]
        .ensure(hasMatchingTag, s"""The tag of a function type must be "$Tag".""")
        .map {
          case (_, attributes, argumentType, returnType) => Type.Function(attributes, argumentType, returnType)
        }
  }

  object Unit extends ExprCompanion("unit") {
    implicit def encodeUnit[A: Encoder]: Encoder[Unit[A]] =
      Encoder.encodeTuple2[String, A].contramap(v => v.tag -> v.attributes)

    implicit def decodeUnit[A: Decoder]: Decoder[Unit[A]] =
      Decoder.decodeTuple2[String, A].ensure(hasMatchingTag, s"""The tag of the unit type must be "$Tag".""").map {
        case (_, attributes) => Unit(attributes)
      }

  }

  sealed abstract class Specification[+A] extends Product with Serializable {
    def mapAttributes[B](f: A => B): Specification[B]
  }
  object Specification {
    final case class TypeAliasSpecification[+A](typeParams: List[Name], typeExp: Type[A]) extends Specification[A] {
      def mapAttributes[B](f: A => B): Specification[B] =
        TypeAliasSpecification(typeParams, typeExp.mapAttributes(f))
    }
    final case class OpaqueTypeSpecification(typeParams: List[Name]) extends Specification[Nothing] {
      def mapAttributes[B](f: Nothing => B): Specification[B] = this
    }
    final case class CustomTypeSpecification[+A](typeParams: List[Name], constructors: Constructors[A])
        extends Specification[A] {
      def mapAttributes[B](f: A => B): Specification[B] =
        CustomTypeSpecification(typeParams, constructors.mapAttributes(f))
    }

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
  object Field {
    implicit def encodeFieldType[A: Encoder]: Encoder[Field[A]] =
      Encoder.encodeTuple2[Name, Type[A]].contramap(ft => ft.name -> ft.fieldType)

    implicit def decodeFieldType[A: Decoder]: Decoder[Field[A]] =
      Decoder.decodeTuple2[Name, Type[A]].map { case (fieldName, fieldType) => Field(fieldName, fieldType) }
  }

  implicit def encodeTypeCoproduct[A: Encoder]: Encoder[Type[A]] = Encoder.instance {
    case variable @ Type.Variable(_, _)                    => variable.asJson
    case reference @ Type.Reference(_, _, _)               => reference.asJson
    case tuple @ Type.Tuple(_, _)                          => tuple.asJson
    case record @ Type.Record(_, _)                        => record.asJson
    case extensibleRecord @ Type.ExtensibleRecord(_, _, _) => extensibleRecord.asJson
    case function @ Type.Function(_, _, _)                 => function.asJson
    case unit @ Type.Unit(_)                               => unit.asJson
  }

  implicit def decodeTypeCoproduct[A: Decoder]: Decoder[Type[A]] =
    Decoder[Variable[A]]
      .widen[Type[A]]
      .or(Decoder[Type.Unit[A]].widen)
      .or(Decoder[Type.Reference[A]].widen)
      .or(Decoder[Type.Tuple[A]].widen)
      .or(Decoder[Type.Record[A]].widen)
      .or(Decoder[Type.ExtensibleRecord[A]].widen)
      .or(Decoder[Type.Function[A]].widen)
}

object Value {

  import syntax.all._

  final case class Literal[+A, +L <: LiteralValue[_]](attributes: A, value: L) extends Value[A](ValueExprKind.Literal) {
    def mapAttributes[B](f: A => B): Value[B] = Literal(f(attributes), value)
  }

  final case class Constructor[+A](attributes: A, fullyQualifiedName: FQName)
      extends Value[A](ValueExprKind.Constructor) {
    def mapAttributes[B](f: A => B): Value[B] = Constructor(f(attributes), fullyQualifiedName)
  }

  final case class Tuple[+A](attributes: A, elements: ValueExprList[A]) extends Value[A](ValueExprKind.Tuple) {
    def mapAttributes[B](f: A => B): Value[B] = Tuple(f(attributes), elements.mapAttributes(f))
  }

  final case class List[+A](attributes: A, items: ValueExprList[A]) extends Value[A](ValueExprKind.List) {
    def mapAttributes[B](f: A => B): Value[B] = List(f(attributes), items.mapAttributes(f))
  }
  final case class Record[+A](attributes: A, fields: RecordFields[A]) extends Value[A](ValueExprKind.Record) {
    def mapAttributes[B](f: A => B): Value[B] = Record(f(attributes), fields.mapAttributes(f))
  }
  final case class Variable[+A](attributes: A, name: Name) extends Value[A](ValueExprKind.Variable) {
    def mapAttributes[B](f: A => B): Value[B] = Variable(f(attributes), name)
  }
  final case class Reference[+A](attributes: A, fullyQualifiedName: FQName) extends Value[A](ValueExprKind.Reference) {
    def mapAttributes[B](f: A => B): Value[B] = Reference(f(attributes), fullyQualifiedName)
  }
  final case class Field[+A](attributes: A, subjectValue: Value[A], fieldName: Name)
      extends Value[A](ValueExprKind.Field) {
    def mapAttributes[B](f: A => B): Value[B] = Field(f(attributes), subjectValue.mapAttributes(f), fieldName)
  }
  final case class FieldFunction[+A](attributes: A, fieldName: Name) extends Value[A](ValueExprKind.FieldFunction) {
    def mapAttributes[B](f: A => B): Value[B] = FieldFunction(f(attributes), fieldName)
  }
  final case class Apply[+A](attributes: A, function: Value[A], argument: Value[A])
      extends Value[A](ValueExprKind.Apply) {
    def mapAttributes[B](f: A => B): Value[B] =
      Apply(f(attributes), function.mapAttributes(f), argument.mapAttributes(f))
  }
  final case class Lambda[+A](attributes: A, argumentPattern: Pattern[A], body: Value[A])
      extends Value[A](ValueExprKind.Lambda) {
    def mapAttributes[B](f: A => B): Value[B] =
      Lambda(f(attributes), argumentPattern.mapAttributes(f), body.mapAttributes(f))
  }
  final case class LetDefinition[+A](attributes: A, valueName: Name, valueDefinition: Definition[A], inValue: Value[A])
      extends Value[A](ValueExprKind.LetDefinition) {
    def mapAttributes[B](f: A => B): Value[B] =
      LetDefinition(f(attributes), valueName, valueDefinition.mapAttributes(f), inValue.mapAttributes(f))
  }
  final case class LetRecursion[+A](attributes: A, valueDefinitions: Map[Name, Definition[A]], inValue: Value[A])
      extends Value[A](ValueExprKind.LetRecursion) {
    def mapAttributes[B](f: A => B): Value[B] =
      LetRecursion(f(attributes), valueDefinitions.map {
        case (name, definition) => name -> definition.mapAttributes(f)
      }, inValue.mapAttributes(f))
  }
  final case class Destructure[+A](attributes: A, pattern: Pattern[A], valueToDestruct: Value[A], inValue: Value[A])
      extends Value[A](ValueExprKind.Destructure) {
    def mapAttributes[B](f: A => B): Value[B] =
      Destructure(f(attributes), pattern.mapAttributes(f), valueToDestruct.mapAttributes(f), inValue.mapAttributes(f))
  }
  final case class IfThenElse[+A](attributes: A, condition: Value[A], thenBranch: Value[A], elseBranch: Value[A])
      extends Value[A](ValueExprKind.IfThenElse) {
    def mapAttributes[B](f: A => B): Value[B] =
      IfThenElse(f(attributes), condition.mapAttributes(f), thenBranch.mapAttributes(f), elseBranch.mapAttributes(f))
  }
  final case class PatternMatch[A](attributes: A, branchOutOn: Value[A], cases: PatternMatchCases[A])
      extends Value[A](ValueExprKind.PatternMatch) {
    def mapAttributes[B](f: A => B): Value[B] =
      PatternMatch(f(attributes), branchOutOn.mapAttributes(f), cases.mapAttributes(f))
  }
  final case class UpdateRecord[+A](attributes: A, valueToUpdate: Value[A], fieldsToUpdate: RecordFields[A])
      extends Value[A](ValueExprKind.UpdateRecord) {
    def mapAttributes[B](f: A => B): Value[B] =
      UpdateRecord(f(attributes), valueToUpdate.mapAttributes(f), fieldsToUpdate.mapAttributes(f))
  }
  final case class Unit[+A](attributes: A) extends Value[A](ValueExprKind.Unit) {
    def mapAttributes[B](f: A => B): Value[B] = Unit(f(attributes))
  }

  object Literal extends ExprCompanion("literal") {

    implicit def encodeLiteral[A: Encoder, L <: LiteralValue[_]](
      implicit evidence: Encoder[L]
    ): Encoder[Literal[A, L]] =
      Encoder.encodeTuple3[String, A, L].contramap(lit => (Tag, lit.attributes, lit.value))

    implicit def decodeLiteral[A: Decoder, L <: LiteralValue[_]](
      implicit evidence: Decoder[L]
    ): Decoder[Literal[A, L]] =
      Decoder.decodeTuple3[String, A, L].map {
        case (_, attributes, literalValue) => Literal(attributes, literalValue)
      }
  }

  object Unit extends ExprCompanion("unit") {

    implicit def encodeUnit[A: Encoder]: Encoder[Unit[A]] =
      Encoder.encodeTuple2[String, A].contramap(u => (Tag, u.attributes))

    implicit def decodeUnit[A: Decoder]: Decoder[Unit[A]] =
      Decoder.decodeTuple2[String, A].map {
        case (_, attributes) => Unit(attributes)
      }

  }

  final case class Specification[+A](inputs: ParameterList[A], output: Type[A]) {
    def mapAttributes[B](f: A => B): Specification[B] = Specification(inputs.mapAttributes(f), output.mapAttributes(f))
  }
  final case class Definition[+A](valueType: Option[Type[A]], arguments: ArgumentList[A], body: Value[A]) {
    def mapAttributes[B](f: A => B): Definition[B] =
      Definition(valueType.map(_.mapAttributes(f)), arguments.mapAttributes(f), body.mapAttributes(f))
  }
}
