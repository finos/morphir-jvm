package morphir.ir

import morphir.ir.codec.{ `type` => typeCodec }

sealed abstract class Expr[+K <: ExprKind, +A](val kind: K) extends Product with Serializable {
  final def tag: String = kind.tag
  def attributes: A
}

sealed abstract class Type[+A](kind: TypeExprKind) extends Expr[TypeExprKind, A](kind) {
  final def isTypeExpr: Boolean = true
}

sealed abstract class Value[+A](kind: ValueExprKind) extends Expr[ValueExprKind, A](kind) {
  final def isTypeExpr: Boolean = false
}

object Type extends typeCodec.TypeCoproductCodec {

  final case class Variable[+A](name: Name, attributes: A) extends Type[A](TypeExprKind.Variable)
  object Variable                                          extends typeCodec.VariableCodec

  final case class Reference[+A](typeName: FQName, typeParameters: List[Type[A]], attributes: A)
      extends Type[A](TypeExprKind.Reference)

  object Reference extends typeCodec.ReferenceCodec

  final case class Tuple[+A](elementTypes: List[Type[A]], attributes: A) extends Type[A](TypeExprKind.Tuple)
  object Tuple                                                           extends typeCodec.TupleCodec

  final case class Record[+A](fieldTypes: List[Field[A]], attributes: A) extends Type[A](TypeExprKind.Record)
  object Record                                                          extends typeCodec.RecordCodec

  final case class ExtensibleRecord[+A](variableName: Name, fieldTypes: List[Field[A]], attributes: A)
      extends Type[A](TypeExprKind.ExtensibleRecord)
  object ExtensibleRecord extends typeCodec.ExtensibleRecordCodec

  final case class Function[+A](argumentType: Type[A], returnType: Type[A], attributes: A)
      extends Type[A](TypeExprKind.Function)
  object Function extends typeCodec.FunctionCodec

  final case class Unit[+A](attributes: A) extends Type[A](TypeExprKind.Unit)
  object Unit                              extends typeCodec.UnitCodec

  sealed abstract class Specification[+A] extends Product with Serializable
  object Specification {
    final case class TypeAliasSpecification[+A](typeParams: List[Name], typeExp: Type[A]) extends Specification[A]
    final case class OpaqueTypeSpecification(typeParams: List[Name])                      extends Specification[Nothing]
    final case class CustomTypeSpecification[+A](typeParams: List[Name], constructors: Constructors[A])
        extends Specification[A]

  }

  sealed abstract class Definition[+A] extends Product with Serializable
  object Definition {}

  final case class Constructors[+A](ctors: List[Constructor[A]]) extends AnyVal

  final case class Constructor[+A](name: Name, args: List[(Name, Type[A])])

  final case class Field[+A](name: Name, fieldType: Type[A])
  object Field extends typeCodec.FieldCodec
}

object Value {

  final case class Specification[+A]()
  final case class Definition[+A]()
}
