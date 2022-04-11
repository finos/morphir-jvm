package zio.morphir.ir.types.nonrecursive

import zio.Chunk
import zio.morphir.ir.{FQName, Name, NeedsAttributes}

import Type._

trait TypeModuleSyntax {
  val unit: UType                                                      = Type.Unit(())
  final def unit[Attributes](attributes: Attributes): Type[Attributes] = Type.Unit(attributes)

  /**
   * Creates a type variable with the given `name`.
   */
  final def variable[Attributes](name: String, attributes: Attributes)(implicit
      ev: NeedsAttributes[Attributes]
  ): Type[Attributes] = Variable(attributes, Name.fromString(name))

  final def variable[Attributes](name: Name, attributes: Attributes)(implicit
      ev: NeedsAttributes[Attributes]
  ): Type[Attributes] = Variable(attributes, name)

  final def variable(name: String): UType = Variable(Name.fromString(name))
  final def variable(name: Name): UType   = Variable(name)

  final def record(fields: Chunk[Field[UType]]): UType =
    Record((), fields)
  final def record(fields: Field[UType]*): UType =
    Record((), Chunk.fromIterable(fields))

  final def record[Attributes](attributes: Attributes, fields: Chunk[Field[Type[Attributes]]])(implicit
      ev: NeedsAttributes[Attributes]
  ): Type[Attributes] =
    Record(attributes, fields)

  final def record[Attributes](attributes: Attributes, fields: Field[Type[Attributes]]*)(implicit
      ev: NeedsAttributes[Attributes]
  ): Type[Attributes] =
    Record(attributes, Chunk.fromIterable(fields))

  final def tuple(elementTypes: Chunk[UType]): UType =
    Tuple((), elementTypes)
  final def tuple(first: UType, second: UType, rest: UType*): UType =
    Tuple((), Chunk(first, second) ++ Chunk.fromIterable(rest))
  final def tuple[Attributes](attributes: Attributes, elementTypes: Chunk[Type[Attributes]])(implicit
      ev: NeedsAttributes[Attributes]
  ): Type[Attributes] =
    Tuple(attributes, elementTypes)
  final def tuple[Attributes](
      attributes: Attributes,
      first: Type[Attributes],
      second: Type[Attributes],
      rest: Type[Attributes]*
  )(implicit ev: NeedsAttributes[Attributes]): Type[Attributes] =
    Tuple(attributes, Chunk(first, second) ++ Chunk.fromIterable(rest))

  final def curriedFunction(paramTypes: List[UType], returnType: UType): UType = {
    def curry(args: List[UType]): UType = args match {
      case Nil                    => returnType
      case firstArg :: restOfArgs => function1(firstArg, curry(restOfArgs))
    }
    curry(paramTypes)
  }

  final def function1(paramType: UType, returnType: UType): UType =
    Function((), Chunk.single(paramType), returnType)

  final def function(paramTypes: Chunk[UType], returnType: UType): UType =
    Function((), paramTypes, returnType)

  final def function[Attributes](
      attributes: Attributes,
      paramTypes: Chunk[Type[Attributes]],
      returnType: Type[Attributes]
  )(implicit ev: NeedsAttributes[Attributes]): Type[Attributes] =
    Function(attributes, paramTypes, returnType)

  final def function[Attributes](paramTypes: Type[Attributes]*): SyntaxHelper.DefineFunction[Attributes] =
    new SyntaxHelper.DefineFunction(() => Chunk.fromIterable(paramTypes))

  final def extensibleRecord(name: Name, fields: Chunk[Field[UType]]): UType =
    ExtensibleRecord((), name, fields)
  final def extensibleRecord(name: Name, fields: Field[UType]*): UType =
    ExtensibleRecord((), name, Chunk.fromIterable(fields))
  final def extensibleRecord(name: String, fields: Chunk[Field[UType]]): UType =
    ExtensibleRecord((), Name.fromString(name), fields)
  final def extensibleRecord(name: String, fields: Field[UType]*): UType =
    ExtensibleRecord((), Name.fromString(name), Chunk.fromIterable(fields))

  final def extensibleRecord[Attributes](
      attributes: Attributes,
      name: Name,
      fields: Chunk[Field[Type[Attributes]]]
  )(implicit ev: NeedsAttributes[Attributes]): Type[Attributes] =
    ExtensibleRecord(attributes, name, fields)

  final def extensibleRecord[Attributes](
      attributes: Attributes,
      name: Name,
      fields: Field[Type[Attributes]]*
  )(implicit ev: NeedsAttributes[Attributes]): Type[Attributes] =
    ExtensibleRecord(attributes, name, Chunk.fromIterable(fields))

  final def extensibleRecord[Attributes](
      attributes: Attributes,
      name: String,
      fields: Chunk[Field[Type[Attributes]]]
  ): Type[Attributes] =
    ExtensibleRecord(attributes, Name.fromString(name), fields)
  final def extensibleRecord[Attributes](
      attributes: Attributes,
      name: String,
      fields: Field[Type[Attributes]]*
  )(implicit ev: NeedsAttributes[Attributes]): Type[Attributes] =
    ExtensibleRecord(attributes, Name.fromString(name), Chunk.fromIterable(fields))

  final def reference[Attributes](attributes: Attributes, fqName: FQName, typeParams: Type[Attributes]*)(implicit
      ev: NeedsAttributes[Attributes]
  ): Type[Attributes] =
    Reference(attributes, fqName, Chunk.fromIterable(typeParams))

  final def reference[Attributes](
      attributes: Attributes,
      fqName: FQName,
      typeParams: Chunk[Type[Attributes]]
  )(implicit ev: NeedsAttributes[Attributes]): Type[Attributes] =
    Reference(attributes, fqName, Chunk.fromIterable(typeParams))

  final def reference[Attributes](
      attributes: Attributes,
      packageName: String,
      moduleName: String,
      localName: String,
      typeParams: Chunk[Type[Attributes]]
  )(implicit ev: NeedsAttributes[Attributes]): Type[Attributes] =
    Reference(attributes, FQName.fqn(packageName, moduleName, localName), typeParams)

  final def reference[Attributes](
      attributes: Attributes,
      packageName: String,
      moduleName: String,
      localName: String,
      typeParams: Type[Attributes]*
  ): Type[Attributes] =
    Reference(attributes, FQName.fqn(packageName, moduleName, localName), Chunk.fromIterable(typeParams))

  final def reference(name: FQName, typeParams: Chunk[UType]): UType =
    Reference((), name, typeParams)

  final def reference(name: FQName, typeParams: UType*): UType =
    Reference((), name, Chunk.fromIterable(typeParams))

  final def reference(
      packageName: String,
      moduleName: String,
      localName: String,
      typeParams: Chunk[UType]
  ): UType =
    Reference((), FQName.fqn(packageName, moduleName, localName), typeParams)

  final def reference(packageName: String, moduleName: String, localName: String, typeParams: UType*): UType =
    Reference((), FQName.fqn(packageName, moduleName, localName), Chunk.fromIterable(typeParams))

  @inline final def ref(name: FQName): UType = reference(name, Chunk.empty)
}
