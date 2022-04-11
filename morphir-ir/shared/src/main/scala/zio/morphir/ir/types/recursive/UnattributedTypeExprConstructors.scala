package zio.morphir.ir.types.recursive

import zio.Chunk
import zio.morphir.ir.{FQName, Name}

trait UnattributedTypeExprConstructors { self =>

  import TypeCase._
  import Type.UType

  final def curriedFunction(paramTypes: List[UType], returnType: UType): UType = {
    def curry(args: List[UType]): UType = args match {
      case Nil                    => returnType
      case firstArg :: restOfArgs => function(firstArg, curry(restOfArgs))
    }
    curry(paramTypes)
  }

  final def extensibleRecord(name: Name, fields: Chunk[Field[UType]]): UType =
    Type(ExtensibleRecordCase((), name, fields))

  final def extensibleRecord(name: String, fields: Chunk[Field[UType]]): UType =
    Type(ExtensibleRecordCase((), Name.fromString(name), fields))

  final def extensibleRecord(name: Name, fields: (String, UType)*): UType = {
    val fieldsChunk = Chunk.fromIterable(fields.map { case (name, typeExpr) => Field(Name.fromString(name), typeExpr) })
    Type(ExtensibleRecordCase((), name, fieldsChunk))
  }

  final def extensibleRecord(name: String, fields: (String, UType)*): UType =
    self.extensibleRecord(Name.fromString(name), fields: _*)

  final def extensibleRecordWithFields(name: Name, fields: Field[UType]*): UType =
    Type(ExtensibleRecordCase((), name, Chunk.fromIterable(fields)))

  final def extensibleRecordWithFields(name: String, fields: Field[UType]*): UType =
    Type(ExtensibleRecordCase((), Name.fromString(name), Chunk.fromIterable(fields)))

  final def function(argumentType: UType, returnType: UType): UType =
    Type(FunctionCase((), argumentType, returnType))

  final def record(fields: Chunk[Field[UType]]): UType =
    Type(RecordCase((), fields))

  final def record(field: Field[UType], fields: Field[UType]*): UType =
    Type(RecordCase((), field +: Chunk.fromIterable(fields)))

  final def record(fields: (String, UType)*): UType =
    Type(
      RecordCase((), Chunk.fromIterable(fields.map { case (name, typeExpr) => Field(Name.fromString(name), typeExpr) }))
    )

  final def reference(typeName: FQName, typeParams: Chunk[UType]): UType =
    Type(ReferenceCase((), typeName, typeParams))

  final def reference(typeName: FQName, typeParams: UType*): UType =
    Type(ReferenceCase((), typeName, Chunk.fromIterable(typeParams)))

  final def reference(typeName: String, typeParams: Chunk[UType]): UType =
    Type(ReferenceCase((), FQName.fromString(typeName), typeParams))

  final def reference(typeName: String, typeParams: UType*): UType =
    Type(ReferenceCase((), FQName.fromString(typeName), Chunk.fromIterable(typeParams)))

  final def reference(packageName: String, moduleName: String, typeName: String, typeParams: UType*): UType =
    Type(ReferenceCase((), FQName.fqn(packageName, moduleName, typeName), Chunk.fromIterable(typeParams)))

  final def reference(packageName: String, moduleName: String, typeName: String, typeParams: Chunk[UType]): UType =
    Type(ReferenceCase((), FQName.fqn(packageName, moduleName, typeName), typeParams))

  final def tuple(elements: UType*): UType =
    Type(TupleCase((), Chunk.fromIterable(elements)))

  final def tuple(elements: Chunk[UType]): UType =
    Type(TupleCase((), elements))

  final val unit: UType = Type(UnitCase(()))

  final def variable(name: Name): UType =
    Type(VariableCase((), name))

  final def variable(name: String): UType =
    Type(VariableCase((), Name.fromString(name)))
}
