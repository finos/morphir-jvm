package zio.morphir.ir.types.recursive

import zio.Chunk
import zio.morphir.ir.{FQName, Name, NeedsAttributes}

trait TypeExprConstructors { self =>
  import TypeCase._
  import Type.FieldT
  // Extensible record constructors

  final def extensibleRecord[A](attributes: A, name: Name, fields: Chunk[FieldT[A]])(implicit
      ev: NeedsAttributes[A]
  ): Type[A] =
    Type(ExtensibleRecordCase(attributes, name, fields))

  final def extensibleRecord[A](attributes: A, name: String, fields: Chunk[FieldT[A]])(implicit
      ev: NeedsAttributes[A]
  ): Type[A] =
    extensibleRecord(attributes, Name.fromString(name), fields)

  final def extensibleRecord[A](attributes: A, name: String, field: FieldT[A], fields: FieldT[A]*)(implicit
      ev: NeedsAttributes[A]
  ): Type[A] =
    extensibleRecord(attributes, Name.fromString(name), field +: Chunk.fromIterable(fields))

  final def extensibleRecord[A](attributes: A, name: Name, fields: (String, Type[A])*)(implicit
      ev: NeedsAttributes[A]
  ): Type[A] = {
    val fieldsChunk = Chunk.fromIterable(fields.map { case (name, typeExpr) =>
      Field(Name.fromString(name), typeExpr)
    })
    Type(ExtensibleRecordCase(attributes, name, fieldsChunk))
  }

  final def extensibleRecord[A](attributes: A, name: String, fields: (String, Type[A])*)(implicit
      ev: NeedsAttributes[A]
  ): Type[A] =
    extensibleRecord(attributes, Name.fromString(name), fields: _*)

  // Function constructors
  final def function[A](attributes: A, argumentType: Type[A], returnType: Type[A])(implicit
      ev: NeedsAttributes[A]
  ): Type[A] =
    Type(FunctionCase(attributes, argumentType, returnType))

  final def record[A](attributes: A, fields: Chunk[Field[Type[A]]])(implicit ev: NeedsAttributes[A]): Type[A] =
    Type(RecordCase(attributes, fields))

  final def reference[A](attributes: A, typeName: FQName, typeParams: Chunk[Type[A]])(implicit
      ev: NeedsAttributes[A]
  ): Type[A] =
    Type(ReferenceCase(attributes, typeName, typeParams))

  final def reference[A](attributes: A, typeName: FQName)(implicit ev: NeedsAttributes[A]): Type[A] =
    Type(ReferenceCase(attributes, typeName, Chunk.empty))

  final def reference[A](attributes: A, typeName: FQName, firstTypeParam: Type[A], otherTypeParams: Type[A]*)(implicit
      ev: NeedsAttributes[A]
  ): Type[A] =
    Type(ReferenceCase(attributes, typeName, Chunk.fromIterable(firstTypeParam +: otherTypeParams)))

  final def reference[A](attributes: A, typeName: String, typeParams: Chunk[Type[A]])(implicit ev: NeedsAttributes[A]) =
    Type(ReferenceCase(attributes, FQName.fromString(typeName), typeParams))

  final def reference[A](attributes: A, typeName: String)(implicit ev: NeedsAttributes[A]) =
    Type(ReferenceCase(attributes, FQName.fromString(typeName), Chunk.empty))

  final def reference[A](attributes: A, typeName: String, firstTypeParam: Type[A], otherTypeParams: Type[A]*)(implicit
      ev: NeedsAttributes[A]
  ) =
    Type(ReferenceCase(attributes, FQName.fromString(typeName), firstTypeParam +: Chunk.fromIterable(otherTypeParams)))

  // Tuple constructors
  final def emptyTuple[A](attributes: A)(implicit ev: NeedsAttributes[A]): Type[A] =
    Type(TupleCase(attributes, Chunk.empty))

  final def tuple[A](attributes: A, elements: Chunk[Type[A]])(implicit ev: NeedsAttributes[A]): Type[A] =
    Type(TupleCase(attributes, elements))

  final def tuple[A](attributes: A, elements: Type[A]*)(implicit ev: NeedsAttributes[A]): Type[A] =
    tuple(attributes, Chunk.fromIterable(elements))

  final def unit[A](attributes: A)(implicit ev: NeedsAttributes[A]): Type[A] = Type(UnitCase(attributes))

  // Variable constructors
  final def variable[A](attributes: A, name: Name)(implicit ev: NeedsAttributes[A]): Type[A] =
    Type(VariableCase(attributes, name))

  final def variable[A](attributes: A, name: String)(implicit ev: NeedsAttributes[A]): Type[A] =
    Type(VariableCase(attributes, Name.fromString(name)))

}
