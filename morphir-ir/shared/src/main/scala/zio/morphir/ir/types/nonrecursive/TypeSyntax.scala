package zio.morphir.ir.types.nonrecursive

import zio.Chunk
import zio.morphir.ir.{FQName, Name}

import Specification.{CustomTypeSpecification, UCustomTypeSpecification}
import Type._

trait TypeSyntax {
  final def customType[Attributes](typeParams: String*)(
      ctors: Constructors[Attributes]
  ): CustomTypeSpecification[Attributes] =
    CustomTypeSpecification(Chunk.fromIterable(typeParams.map(Name.fromString)), ctors)

  final def customType[Attributes](typeParams: String*)(
      ctors: (Name, Chunk[(Name, Type[Attributes])])
  ): CustomTypeSpecification[Attributes] =
    CustomTypeSpecification(Chunk.fromIterable(typeParams.map(Name.fromString)), Constructors[Attributes](Map(ctors)))

  final def customType[Attributes](
      ctors: (Name, Chunk[(Name, Type[Attributes])])
  ): CustomTypeSpecification[Attributes] =
    CustomTypeSpecification(Chunk.empty, Constructors[Attributes](Map(ctors)))

  final def defineVariable(name: String): UType = Variable((), Name.fromString(name))
  final def defineVariable(name: Name): UType   = Variable((), name)

  final def defineRecord(fields: Chunk[Field[UType]]): UType =
    Record((), fields)
  final def defineRecord(fields: Field[UType]*): UType =
    Record((), Chunk.fromIterable(fields))

  final def defineTuple(elementTypes: Chunk[UType]): UType =
    Tuple((), elementTypes)
  final def defineTuple(first: UType, second: UType, rest: UType*): UType =
    Tuple((), Chunk(first, second) ++ Chunk.fromIterable(rest))

  final def defineFunction(paramTypes: Chunk[UType], returnType: UType): UType =
    Function((), paramTypes, returnType)
  final def defineFunction[Annotations](paramTypes: Type[Annotations]*): SyntaxHelper.DefineFunction[Annotations] =
    new SyntaxHelper.DefineFunction(() => Chunk.fromIterable(paramTypes))

  final def defineExtensibleRecord(name: Name, fields: Chunk[Field[UType]]): UType =
    ExtensibleRecord((), name, fields)
  final def defineExtensibleRecord(name: Name, fields: Field[UType]*): UType =
    ExtensibleRecord((), name, Chunk.fromIterable(fields))
  final def defineExtensibleRecord(name: String, fields: Chunk[Field[UType]]): UType =
    ExtensibleRecord((), Name.fromString(name), fields)
  final def defineExtensibleRecord(name: String, fields: Field[UType]*): UType =
    ExtensibleRecord((), Name.fromString(name), Chunk.fromIterable(fields))

  final def defineReference(name: FQName, typeParams: Chunk[UType]): UType =
    Reference((), name, typeParams)
  final def defineReference(name: FQName, typeParams: UType*): UType =
    Reference((), name, Chunk.fromIterable(typeParams))
  final def defineReference(
      packageName: String,
      moduleName: String,
      localName: String,
      typeParams: Chunk[UType]
  ): UType =
    Reference((), FQName.fqn(packageName, moduleName, localName), typeParams)
  final def defineReference(packageName: String, moduleName: String, localName: String, typeParams: UType*): UType =
    Reference((), FQName.fqn(packageName, moduleName, localName), Chunk.fromIterable(typeParams))

  final def enumType(case1: String, otherCases: String*): UCustomTypeSpecification =
    UCustomTypeSpecification.mkEnum(case1, otherCases: _*)

  final def typeConstructor[Attributes](
      name: String,
      args: (String, Type[Attributes])*
  ): (Name, Chunk[(Name, Type[Attributes])]) =
    (Name.fromString(name), Chunk.fromIterable(args.map { case (name, tpe) => (Name.fromString(name), tpe) }))

  @inline final def tCtor[Attributes](
      name: String,
      args: (String, Type[Attributes])*
  ): (Name, Chunk[(Name, Type[Attributes])]) =
    typeConstructor(name, args: _*)

  val unitType: UType                                                      = Unit(())
  final def unitType[Attributes](attributes: Attributes): Type[Attributes] = Unit(attributes)
}
