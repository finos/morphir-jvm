package zio.morphir.syntax

import zio.Chunk
import zio.morphir.ir.TypeModule.Specification.{CustomTypeSpecification, UCustomTypeSpecification}
import zio.morphir.ir.TypeModule.TypeCase._
import zio.morphir.ir.TypeModule.{Field, Type}
import zio.morphir.ir.{FQName, Name, TypeConstructors, UType}

trait TypeSyntax {
  def customType[Attributes](typeParams: String*)(
      ctors: TypeConstructors[Attributes]
  ): CustomTypeSpecification[Attributes] =
    CustomTypeSpecification(Chunk.fromIterable(typeParams.map(Name.fromString)), ctors)

  def customType[Attributes](typeParams: String*)(
      ctors: (Name, Chunk[(Name, Type[Attributes])])
  ): CustomTypeSpecification[Attributes] =
    CustomTypeSpecification(Chunk.fromIterable(typeParams.map(Name.fromString)), TypeConstructors(Map(ctors)))

  def customType[Attributes](
      ctors: (Name, Chunk[(Name, Type[Attributes])])
  ): CustomTypeSpecification[Attributes] =
    CustomTypeSpecification(Chunk.empty, TypeConstructors(Map(ctors)))

  def defineVariable(name: String): UType = Type(VariableCase(Name.fromString(name)), Type.emptyAttributes)
  def defineVariable(name: Name): UType   = Type(VariableCase(name), Type.emptyAttributes)

  def defineField(name: Name, fieldType: UType): Field[UType]   = Field(name, fieldType)
  def defineField(name: String, fieldType: UType): Field[UType] = Field(Name.fromString(name), fieldType)

  def defineRecord(fields: Chunk[Field[UType]]): UType =
    Type(RecordCase(fields), Type.emptyAttributes)
  def defineRecord(fields: Field[UType]*): UType =
    Type(RecordCase(Chunk.fromIterable(fields)), Type.emptyAttributes)

  def defineTuple(elementTypes: Chunk[UType]): UType =
    Type(TupleCase(elementTypes), Type.emptyAttributes)
  def defineTuple(first: UType, second: UType, rest: UType*): UType =
    Type(TupleCase(Chunk(first, second) ++ Chunk.fromIterable(rest)), Type.emptyAttributes)

  def defineFunction(paramTypes: Chunk[UType], returnType: UType): UType =
    Type(FunctionCase(paramTypes, returnType), Type.emptyAttributes)
  def defineFunction[Annotations](paramTypes: Type[Annotations]*): SyntaxHelper.DefineFunction[Annotations] =
    new SyntaxHelper.DefineFunction(() => Chunk.fromIterable(paramTypes))

  def defineExtensibleRecord(name: Name, fields: Chunk[Field[UType]]): UType =
    Type(ExtensibleRecordCase(name, fields), Type.emptyAttributes)
  def defineExtensibleRecord(name: Name, fields: Field[UType]*): UType =
    Type(ExtensibleRecordCase(name, Chunk.fromIterable(fields)), Type.emptyAttributes)
  def defineExtensibleRecord(name: String, fields: Chunk[Field[UType]]): UType =
    Type(ExtensibleRecordCase(Name.fromString(name), fields), Type.emptyAttributes)
  def defineExtensibleRecord(name: String, fields: Field[UType]*): UType =
    Type(ExtensibleRecordCase(Name.fromString(name), Chunk.fromIterable(fields)), Type.emptyAttributes)

  def defineReference(name: FQName, typeParams: Chunk[UType]): UType =
    Type(ReferenceCase(name, typeParams), Type.emptyAttributes)
  def defineReference(name: FQName, typeParams: UType*): UType =
    Type(ReferenceCase(name, Chunk.fromIterable(typeParams)), Type.emptyAttributes)
  def defineReference(
      packageName: String,
      moduleName: String,
      localName: String,
      typeParams: Chunk[UType]
  ): UType =
    Type(ReferenceCase(FQName.fqn(packageName, moduleName, localName), typeParams), Type.emptyAttributes)
  def defineReference(packageName: String, moduleName: String, localName: String, typeParams: UType*): UType =
    Type(
      ReferenceCase(FQName.fqn(packageName, moduleName, localName), Chunk.fromIterable(typeParams)),
      Type.emptyAttributes
    )

  def enumType(case1: String, otherCases: String*): UCustomTypeSpecification =
    UCustomTypeSpecification.mkEnum(case1, otherCases: _*)

  def typeConstructor[Attributes](
      name: String,
      args: (String, Type[Attributes])*
  ): (Name, Chunk[(Name, Type[Attributes])]) =
    (Name.fromString(name), Chunk.fromIterable(args.map { case (name, tpe) => (Name.fromString(name), tpe) }))

  @inline def tCtor[Attributes](
      name: String,
      args: (String, Type[Attributes])*
  ): (Name, Chunk[(Name, Type[Attributes])]) =
    typeConstructor(name, args: _*)
}

trait TypeModuleSyntax {
  val unit: UType                                                          = Type(UnitCase, ())
  final def unit[Annotations](annotations: Annotations): Type[Annotations] = Type(UnitCase, annotations)

  /**
   * Creates a type variable with the given `name`.
   */
  final def variable[Attributes](name: String, attributes: Attributes): Type[Attributes] =
    Type(VariableCase(Name.fromString(name)), attributes)
  final def variable[Attributes](name: Name, attributes: Attributes): Type[Attributes] =
    Type(VariableCase(name), attributes)
  final def variable(name: String): UType = Type(VariableCase(Name.fromString(name)))
  final def variable(name: Name): UType   = Type(VariableCase(name))

  final def field(name: Name, fieldType: UType): Field[UType]   = Field(name, fieldType)
  final def field(name: String, fieldType: UType): Field[UType] = Field(Name.fromString(name), fieldType)

  final def record(fields: Chunk[Field[UType]]): UType =
    Type(RecordCase(fields), Type.emptyAttributes)
  final def record(fields: Field[UType]*): UType =
    Type(RecordCase(Chunk.fromIterable(fields)), Type.emptyAttributes)

  final def tuple(elementTypes: Chunk[UType]): UType =
    Type(TupleCase(elementTypes), Type.emptyAttributes)
  final def tuple(first: UType, second: UType, rest: UType*): UType =
    Type(TupleCase(Chunk(first, second) ++ Chunk.fromIterable(rest)), Type.emptyAttributes)

  def curriedFunction(paramTypes: List[UType], returnType: UType): UType = {
    def curry(args: List[UType]): UType = args match {
      case Nil                    => returnType
      case firstArg :: restOfArgs => function1(firstArg, curry(restOfArgs))
    }
    curry(paramTypes)
  }

  final def function1(paramType: UType, returnType: UType): UType =
    Type(FunctionCase(Chunk.single(paramType), returnType), Type.emptyAttributes)

  final def function(paramTypes: Chunk[UType], returnType: UType): UType =
    Type(FunctionCase(paramTypes, returnType), Type.emptyAttributes)

  final def function[Annotations](paramTypes: Type[Annotations]*): SyntaxHelper.DefineFunction[Annotations] =
    new SyntaxHelper.DefineFunction(() => Chunk.fromIterable(paramTypes))

  final def extensibleRecord(name: Name, fields: Chunk[Field[UType]]): UType =
    Type(ExtensibleRecordCase(name, fields), Type.emptyAttributes)
  final def extensibleRecord(name: Name, fields: Field[UType]*): UType =
    Type(ExtensibleRecordCase(name, Chunk.fromIterable(fields)), Type.emptyAttributes)
  final def extensibleRecord(name: String, fields: Chunk[Field[UType]]): UType =
    Type(ExtensibleRecordCase(Name.fromString(name), fields), Type.emptyAttributes)
  final def extensibleRecord(name: String, fields: Field[UType]*): UType =
    Type(ExtensibleRecordCase(Name.fromString(name), Chunk.fromIterable(fields)), Type.emptyAttributes)

  final def reference[Attributes](
      attributes: Attributes
  )(fqName: FQName, typeParams: Type[Attributes]*): Type[Attributes] =
    Type(ReferenceCase(fqName, Chunk.fromIterable(typeParams)), attributes)

  final def reference(name: FQName, typeParams: Chunk[UType]): UType =
    Type(ReferenceCase(name, typeParams), Type.emptyAttributes)

  final def reference(name: FQName, typeParams: UType*): UType =
    Type(ReferenceCase(name, Chunk.fromIterable(typeParams)), Type.emptyAttributes)

  final def reference(
      packageName: String,
      moduleName: String,
      localName: String,
      typeParams: Chunk[UType]
  ): UType =
    Type(ReferenceCase(FQName.fqn(packageName, moduleName, localName), typeParams), Type.emptyAttributes)

  def reference(packageName: String, moduleName: String, localName: String, typeParams: UType*): UType =
    Type(
      ReferenceCase(FQName.fqn(packageName, moduleName, localName), Chunk.fromIterable(typeParams)),
      Type.emptyAttributes
    )

  @inline final def ref(name: FQName): UType = reference(name, Chunk.empty)
}

object SyntaxHelper {
  final class DefineFunction[Annotations](val paramTypes: () => Chunk[Type[Annotations]]) extends AnyVal {
    def apply(returnType: Type[Annotations], annotations: Annotations): Type[Annotations] =
      Type(FunctionCase(paramTypes(), returnType), annotations)
  }

}
