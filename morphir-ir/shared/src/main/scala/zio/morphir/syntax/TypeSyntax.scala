package zio.morphir.syntax

import zio.Chunk
import zio.morphir.ir.Name
import zio.morphir.ir.FQName
import zio.morphir.ir.TypeModule.Type
import zio.morphir.ir.TypeModule.Type.*
import zio.ZEnvironment

trait TypeSyntax {
  def defineVariable(name: String): Variable[Any]               = Variable(Name.fromString(name), ZEnvironment.empty)
  def defineVariable(name: Name): Variable[Any]                 = Variable(name, ZEnvironment.empty)
  def defineField(name: Name, fieldType: Type[Any]): Field[Any] = Field(name, fieldType, ZEnvironment.empty)
  def defineRecord(fields: Chunk[Field[Any]]): Record[Any]      = Record(fields, ZEnvironment.empty)
  def defineTuple(elementTypes: Chunk[Type[Any]]): Tuple[Any]   = Tuple(elementTypes, ZEnvironment.empty)
  def defineFunction(paramTypes: Chunk[Type[Any]], returnType: Type[Any]): Function[Any] =
    Function(paramTypes, returnType, ZEnvironment.empty)
  def defineExtensibleRecord(name: Name, fields: Chunk[Field[Any]]): ExtensibleRecord[Any] =
    ExtensibleRecord(name, fields, ZEnvironment.empty)
  def defineReference(name: FQName, typeParams: Chunk[Type[Any]]): Reference[Any] =
    Reference(name, typeParams, ZEnvironment.empty)
}

trait TypeModuleSyntax {
  val unit: Unit[Any]                                     = Unit(ZEnvironment.empty)
  def variable(name: String): Variable[Any]               = Variable(Name.fromString(name), ZEnvironment.empty)
  def variable(name: Name): Variable[Any]                 = Variable(name, ZEnvironment.empty)
  def field(name: Name, fieldType: Type[Any]): Field[Any] = Field(name, fieldType, ZEnvironment.empty)
  def record(fields: Chunk[Field[Any]]): Record[Any]      = Record(fields, ZEnvironment.empty)
  def tuple(elementTypes: Chunk[Type[Any]]): Tuple[Any]   = Tuple(elementTypes, ZEnvironment.empty)
  def function(paramTypes: Chunk[Type[Any]], returnType: Type[Any]): Function[Any] =
    Function(paramTypes, returnType, ZEnvironment.empty)
  def extensibleRecord(name: Name, fields: Chunk[Field[Any]]): ExtensibleRecord[Any] =
    ExtensibleRecord(name, fields, ZEnvironment.empty)
  def reference(name: FQName, typeParams: Chunk[Type[Any]]): Reference[Any] =
    Reference(name, typeParams, ZEnvironment.empty)
}
