package zio.morphir.syntax

import zio.morphir.ir.TypeModule.TypeCase.*
import zio.morphir.ir.TypeModule.{Field, Type}
import zio.morphir.ir.{FQName, Name}
import zio.{Chunk, ZEnvironment}

trait TypeSyntax {
  def defineVariable(name: String): Type[Any] = Type(VariableCase(Name.fromString(name)), ZEnvironment.empty)
  def defineVariable(name: Name): Type[Any]   = Type(VariableCase(name), ZEnvironment.empty)

  def defineField(name: Name, fieldType: Type[Any]): Field[Type[Any]]   = Field(name, fieldType)
  def defineField(name: String, fieldType: Type[Any]): Field[Type[Any]] = Field(Name.fromString(name), fieldType)

  def defineRecord(fields: Chunk[Field[Type[Any]]]): Type[Any] =
    Type(RecordCase(fields), ZEnvironment.empty)
  def defineRecord(fields: Field[Type[Any]]*): Type[Any] =
    Type(RecordCase(Chunk.fromIterable(fields)), ZEnvironment.empty)

  def defineTuple(elementTypes: Chunk[Type[Any]]): Type[Any] =
    Type(TupleCase(elementTypes), ZEnvironment.empty)
  def defineTuple(first: Type[Any], second: Type[Any], rest: Type[Any]*): Type[Any] =
    Type(TupleCase(Chunk(first, second) ++ Chunk.fromIterable(rest)), ZEnvironment.empty)

  def defineFunction(paramTypes: Chunk[Type[Any]], returnType: Type[Any]): Type[Any] =
    Type(FunctionCase(paramTypes, returnType), ZEnvironment.empty)
  def defineFunction[Annotations](paramTypes: Type[Annotations]*): SyntaxHelper.DefineFunction[Annotations] =
    new SyntaxHelper.DefineFunction(() => Chunk.fromIterable(paramTypes))

  def defineExtensibleRecord(name: Name, fields: Chunk[Field[Type[Any]]]): Type[Any] =
    Type(ExtensibleRecordCase(name, fields), ZEnvironment.empty)
  def defineExtensibleRecord(name: Name, fields: Field[Type[Any]]*): Type[Any] =
    Type(ExtensibleRecordCase(name, Chunk.fromIterable(fields)), ZEnvironment.empty)
  def defineExtensibleRecord(name: String, fields: Chunk[Field[Type[Any]]]): Type[Any] =
    Type(ExtensibleRecordCase(Name.fromString(name), fields), ZEnvironment.empty)
  def defineExtensibleRecord(name: String, fields: Field[Type[Any]]*): Type[Any] =
    Type(ExtensibleRecordCase(Name.fromString(name), Chunk.fromIterable(fields)), ZEnvironment.empty)

  def defineReference(name: FQName, typeParams: Chunk[Type[Any]]): Type[Any] =
    Type(ReferenceCase(name, typeParams), ZEnvironment.empty)
  def defineReference(name: FQName, typeParams: Type[Any]*): Type[Any] =
    Type(ReferenceCase(name, Chunk.fromIterable(typeParams)), ZEnvironment.empty)
  def defineReference(
      packageName: String,
      moduleName: String,
      localName: String,
      typeParams: Chunk[Type[Any]]
  ): Type[Any] =
    Type(ReferenceCase(FQName.fqn(packageName, moduleName, localName), typeParams), ZEnvironment.empty)
  def defineReference(packageName: String, moduleName: String, localName: String, typeParams: Type[Any]*): Type[Any] =
    Type(
      ReferenceCase(FQName.fqn(packageName, moduleName, localName), Chunk.fromIterable(typeParams)),
      ZEnvironment.empty
    )
}

trait TypeModuleSyntax {
  val unit: Type[Any] = Type(UnitCase, ZEnvironment.empty)
  final def unit[Annotations](annotations: ZEnvironment[Annotations]): Type[Annotations] = Type(UnitCase, annotations)

  /**
   * Creates a type variable with the given `name`.
   */
  final def variable(name: String): Type[Any] = Type(VariableCase(Name.fromString(name)), ZEnvironment.empty)
  final def variable(name: Name): Type[Any]   = Type(VariableCase(name), ZEnvironment.empty)

  final def field(name: Name, fieldType: Type[Any]): Field[Type[Any]]   = Field(name, fieldType)
  final def field(name: String, fieldType: Type[Any]): Field[Type[Any]] = Field(Name.fromString(name), fieldType)

  final def record(fields: Chunk[Field[Type[Any]]]): Type[Any] =
    Type(RecordCase(fields), ZEnvironment.empty)
  final def record(fields: Field[Type[Any]]*): Type[Any] =
    Type(RecordCase(Chunk.fromIterable(fields)), ZEnvironment.empty)

  final def tuple(elementTypes: Chunk[Type[Any]]): Type[Any] =
    Type(TupleCase(elementTypes), ZEnvironment.empty)
  final def tuple(first: Type[Any], second: Type[Any], rest: Type[Any]*): Type[Any] =
    Type(TupleCase(Chunk(first, second) ++ Chunk.fromIterable(rest)), ZEnvironment.empty)

  final def function(paramTypes: Chunk[Type[Any]], returnType: Type[Any]): Type[Any] =
    Type(FunctionCase(paramTypes, returnType), ZEnvironment.empty)
  final def function[Annotations](paramTypes: Type[Annotations]*): SyntaxHelper.DefineFunction[Annotations] =
    new SyntaxHelper.DefineFunction(() => Chunk.fromIterable(paramTypes))

  final def extensibleRecord(name: Name, fields: Chunk[Field[Type[Any]]]): Type[Any] =
    Type(ExtensibleRecordCase(name, fields), ZEnvironment.empty)
  final def extensibleRecord(name: Name, fields: Field[Type[Any]]*): Type[Any] =
    Type(ExtensibleRecordCase(name, Chunk.fromIterable(fields)), ZEnvironment.empty)
  final def extensibleRecord(name: String, fields: Chunk[Field[Type[Any]]]): Type[Any] =
    Type(ExtensibleRecordCase(Name.fromString(name), fields), ZEnvironment.empty)
  final def extensibleRecord(name: String, fields: Field[Type[Any]]*): Type[Any] =
    Type(ExtensibleRecordCase(Name.fromString(name), Chunk.fromIterable(fields)), ZEnvironment.empty)

  final def reference(name: FQName, typeParams: Chunk[Type[Any]]): Type[Any] =
    Type(ReferenceCase(name, typeParams), ZEnvironment.empty)
  final def reference(name: FQName, typeParams: Type[Any]*): Type[Any] =
    Type(ReferenceCase(name, Chunk.fromIterable(typeParams)), ZEnvironment.empty)
  final def reference(
      packageName: String,
      moduleName: String,
      localName: String,
      typeParams: Chunk[Type[Any]]
  ): Type[Any] =
    Type(ReferenceCase(FQName.fqn(packageName, moduleName, localName), typeParams), ZEnvironment.empty)

  def reference(packageName: String, moduleName: String, localName: String, typeParams: Type[Any]*): Type[Any] =
    Type(
      ReferenceCase(FQName.fqn(packageName, moduleName, localName), Chunk.fromIterable(typeParams)),
      ZEnvironment.empty
    )

  @inline final def ref(name: FQName): Type[Any] = reference(name, Chunk.empty)
}

object SyntaxHelper {
  final class DefineFunction[Annotations](val paramTypes: () => Chunk[Type[Annotations]]) extends AnyVal {
    def apply(returnType: Type[Annotations], annotations: ZEnvironment[Annotations]): Type[Annotations] =
      Type(FunctionCase(paramTypes(), returnType), annotations)
  }
}
