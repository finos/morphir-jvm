package zio.morphir.ir.types

import zio.{Chunk, ZIO}
import zio.morphir.ir.{Documented, FQName, Name}
import zio.prelude._
import zio.prelude.fx._

import scala.annotation.tailrec

sealed trait Type[+Attributes] { self =>
  def @@[Attributes2](f: Attributes => Attributes2): Type[Attributes2] =
    mapAttributes(f)

  def ??(doc: String): Documented[Type[Attributes]] = Documented(doc, self)

  def attributes: Attributes

  def collect[Z](pf: PartialFunction[Type[Attributes], Z]): Chunk[Z] =
    foldLeft[Chunk[Z]](Chunk.empty) { case (acc, tpe) =>
      if (pf.isDefinedAt(tpe)) acc :+ pf(tpe)
      else acc
    }

  // TODO: See if we can refactor to be stack safe/ tail recursive
  def fold[Z](
      extensibleRecordCase: (Attributes, Name, Chunk[Field[Z]]) => Z,
      functionCase: (Attributes, Chunk[Z], Z) => Z,
      recordCase: (Attributes, Chunk[Field[Z]]) => Z,
      referenceCase: (Attributes, FQName, Chunk[Z]) => Z,
      tupleCase: (Attributes, Chunk[Z]) => Z,
      variableCase: (Attributes, Name) => Z,
      unitCase: Attributes => Z
  ): Z =
    self match {
      case Type.ExtensibleRecord(attributes, name, fields) =>
        extensibleRecordCase(
          attributes,
          name,
          fields.map(
            _.map(
              _.fold(extensibleRecordCase, functionCase, recordCase, referenceCase, tupleCase, variableCase, unitCase)
            )
          )
        )
      case Type.Function(attributes, paramTypes, returnType) =>
        functionCase(
          attributes,
          paramTypes.map(
            _.fold(extensibleRecordCase, functionCase, recordCase, referenceCase, tupleCase, variableCase, unitCase)
          ),
          returnType.fold(
            extensibleRecordCase,
            functionCase,
            recordCase,
            referenceCase,
            tupleCase,
            variableCase,
            unitCase
          )
        )
      case Type.Record(attributes, fields) =>
        recordCase(
          attributes,
          fields.map(
            _.map(
              _.fold(extensibleRecordCase, functionCase, recordCase, referenceCase, tupleCase, variableCase, unitCase)
            )
          )
        )
      case Type.Reference(attributes, typeName, typeParams) =>
        referenceCase(
          attributes,
          typeName,
          typeParams.map(
            _.fold(extensibleRecordCase, functionCase, recordCase, referenceCase, tupleCase, variableCase, unitCase)
          )
        )
      case Type.Tuple(attributes, elementTypes) =>
        tupleCase(
          attributes,
          elementTypes.map(
            _.fold(extensibleRecordCase, functionCase, recordCase, referenceCase, tupleCase, variableCase, unitCase)
          )
        )
      case Type.Unit(attributes)           => unitCase(attributes)
      case Type.Variable(attributes, name) => variableCase(attributes, name)
    }

//  def identity: Type[Attributes] =
//    self.fold[Type[Attributes]](
//      (attributes, name, fields) => Type.ExtensibleRecord(attributes, name, fields),
//      (attributes, paramTypes, returnType) => Type.Function(attributes, paramTypes, returnType),
//      (attributes, fields) => Type.Record(attributes, fields),
//      (attributes, typeName, typeParams) => Type.Reference(attributes, typeName, typeParams),
//      (attributes, elements) => Type.Tuple(attributes, elements),
//      (attributes, name) => Type.Variable(attributes, name),
//      attributes => Type.Unit(attributes)
//    )

  def foldLeft[Z](zero: Z)(f: (Z, Type[Attributes]) => Z): Z = {
    @tailrec
    def loop(types: List[Type[Attributes]], acc: Z): Z = types match {
      case (tpe @ Type.ExtensibleRecord(_, _, _)) :: tail =>
        loop(tpe.fields.map(_.fieldType).toList ++ tail, f(acc, tpe))
      case (tpe @ Type.Function(_, _, _)) :: tail =>
        loop(tpe.paramTypes.toList ++ (tpe.returnType :: tail), f(acc, tpe))
      case (tpe @ Type.Record(_, _)) :: tail =>
        loop(tpe.fields.map(_.fieldType).toList ++ tail, f(acc, tpe))
      case (tpe @ Type.Reference(_, _, _)) :: tail =>
        loop(tpe.typeParams.toList ++ tail, f(acc, tpe))
      case (tpe @ Type.Tuple(_, elements)) :: tail =>
        loop(elements.toList ++ tail, f(acc, tpe))
      case (tpe @ Type.Variable(_, _)) :: tail => loop(tail, f(acc, tpe))
      case (tpe @ Type.Unit(_)) :: tail        => loop(tail, f(acc, tpe))
      case Nil                                 => acc
    }
    loop(List(self), zero)
  }

  def foldDown[Z](z: Z)(f: (Z, Type[Attributes]) => Z): Z =
    foldLeft(f(z, self))((z, recursive) => recursive.foldDown(z)(f))

  def foldDownSome[Z](z: Z)(pf: PartialFunction[(Z, Type[Attributes]), Z]): Z =
    foldDown(z)((z, recursive) => pf.lift(z -> recursive).getOrElse(z))

  def foldM[F[+_]: AssociativeFlatten: Covariant: IdentityBoth, Z](
      extensibleRecordCase: (Attributes, Name, Chunk[Field[Z]]) => F[Z],
      functionCase: (Attributes, Chunk[Z], Z) => F[Z],
      recordCase: (Attributes, Chunk[Field[Z]]) => F[Z],
      referenceCase: (Attributes, FQName, Chunk[Z]) => F[Z],
      tupleCase: (Attributes, Chunk[Z]) => F[Z],
      variableCase: (Attributes, Name) => F[Z],
      unitCase: Attributes => F[Z]
  ): F[Z] =
    fold[F[Z]](
      (
          attributes,
          name,
          fields
      ) => fields.forEach(_.forEach(a => a)).flatMap(extensibleRecordCase(attributes, name, _)),
      (attributes, paramTypes, returnType) =>
        for {
          paramTypes <- paramTypes.flip
          returnType <- returnType
          z          <- functionCase(attributes, paramTypes, returnType)
        } yield z,
      (attributes, fields) => fields.forEach(_.forEach(a => a)).flatMap(recordCase(attributes, _)),
      (attributes, typeName, typeParams) => typeParams.flip.flatMap(referenceCase(attributes, typeName, _)),
      (attributes, elements) => elements.flip.flatMap(tupleCase(attributes, _)),
      (attributes, name) => variableCase(attributes, name),
      attributes => unitCase(attributes)
    )

  def foldPure[W, S, R, E, Z](
      extensibleRecordCase: (Attributes, Name, Chunk[Field[Z]]) => ZPure[W, S, S, R, E, Z],
      functionCase: (Attributes, Chunk[Z], Z) => ZPure[W, S, S, R, E, Z],
      recordCase: (Attributes, Chunk[Field[Z]]) => ZPure[W, S, S, R, E, Z],
      referenceCase: (Attributes, FQName, Chunk[Z]) => ZPure[W, S, S, R, E, Z],
      tupleCase: (Attributes, Chunk[Z]) => ZPure[W, S, S, R, E, Z],
      variableCase: (Attributes, Name) => ZPure[W, S, S, R, E, Z],
      unitCase: Attributes => ZPure[W, S, S, R, E, Z]
  ): ZPure[W, S, S, R, E, Z] = foldM(
    extensibleRecordCase,
    functionCase,
    recordCase,
    referenceCase,
    tupleCase,
    variableCase,
    unitCase
  )
  //
//      def transformDown[Attributes1 >: Attributes](
//          f: Type[Attributes1] => Type[Attributes1]
//      ): Type[Attributes1] =
//        f(self) match {
//          case Type.ExtensibleRecord(attributes, name, fields) => Type.ExtensibleRecord(attributes, name, fields.map(_.map(_.transformDown(f))))
//          case Type.Function(attributes, paramTypes, returnType) => Type.Function(attributes, paramTypes.map(_.transformDown(f)), returnType.transformDown(f))
//          case Type.Record(attributes, fields) => Type.Record(attributes, fields.map(_.map(_.transformDown(f))))
//          case Type.Reference(attributes, typeName, typeParams) =>
//            Type.Reference(attributes, typeName, typeParams.map(_.transformDown(f)))
//          case Type.Tuple(attributes, elementTypes) => Type.Tuple(attributes, elementTypes.map(_.transformDown(f)))
//          case Type.Unit(attributes) => Type.Unit(attributes)
//          case Type.Variable(attributes, name) => Type.Variable(attributes, name)
//        }

  def transform[Attributes1 >: Attributes](f: Type[Attributes1] => Type[Attributes1]): Type[Attributes1] =
    fold[Type[Attributes1]](
      (attributes, name, fields) => f(Type.ExtensibleRecord(attributes, name, fields)),
      (attributes, paramTypes, returnType) => f(Type.Function(attributes, paramTypes, returnType)),
      (attributes, fields) => f(Type.Record(attributes, fields)),
      (attributes, typeName, typeParams) => f(Type.Reference(attributes, typeName, typeParams)),
      (attributes, elements) => f(Type.Tuple(attributes, elements)),
      (attributes, name) => f(Type.Variable(attributes, name)),
      attributes => f(Type.Unit(attributes))
    )

  def rewrite[Attributes1 >: Attributes](pf: PartialFunction[Type[Attributes1], Type[Attributes1]]): Type[Attributes1] =
    transform[Attributes1](in => pf.lift(in).getOrElse(in))
  //
  def foldZIO[R, E, Z](
      extensibleRecordCase: (Attributes, Name, Chunk[Field[Z]]) => ZIO[R, E, Z],
      functionCase: (Attributes, Chunk[Z], Z) => ZIO[R, E, Z],
      recordCase: (Attributes, Chunk[Field[Z]]) => ZIO[R, E, Z],
      referenceCase: (Attributes, FQName, Chunk[Z]) => ZIO[R, E, Z],
      tupleCase: (Attributes, Chunk[Z]) => ZIO[R, E, Z],
      variableCase: (Attributes, Name) => ZIO[R, E, Z],
      unitCase: Attributes => ZIO[R, E, Z]
  ): ZIO[R, E, Z] =
    foldM(extensibleRecordCase, functionCase, recordCase, referenceCase, tupleCase, variableCase, unitCase)

  def foldUp[Z](z: Z)(f: (Z, Type[Attributes]) => Z): Z =
    f(self.foldLeft(z)((z, recursive) => recursive.foldUp(z)(f)), self)

  def foldUpSome[Z](z: Z)(pf: PartialFunction[(Z, Type[Attributes]), Z]): Z =
    foldUp(z)((z, recursive) => pf.lift(z -> recursive).getOrElse(z))

  def mapAttributes[Attributes2](f: Attributes => Attributes2): Type[Attributes2] =
    fold[Type[Attributes2]](
      (attributes, name, fields) => Type.ExtensibleRecord(f(attributes), name, fields),
      (attributes, paramTypes, returnType) => Type.Function(f(attributes), paramTypes, returnType),
      (attributes, fields) => Type.Record(f(attributes), fields),
      (attributes, typeName, typeParams) => Type.Reference(f(attributes), typeName, typeParams),
      (attributes, elements) => Type.Tuple(f(attributes), elements),
      (attributes, name) => Type.Variable(f(attributes), name),
      attributes => Type.Unit(f(attributes))
    )

  def collectReferences: Set[FQName] = foldLeft(Set.empty[FQName]) { case (acc, tpe) =>
    tpe match {
      case Type.Reference(_, typeName, _) => acc + typeName
      case _                              => acc
    }
  }

  def collectVariables: Set[Name] = foldLeft(Set.empty[Name]) { case (acc, tpe) =>
    tpe match {
      case tpe @ Type.ExtensibleRecord(_, _, _) => acc + tpe.name
      case Type.Variable(_, name)               => acc + name
      case _                                    => acc
    }
  }

  /**
   * Erase the attributes from this type.
   */
  def eraseAttributes: UType = self.mapAttributes(_ => ())

  // TO DO
  // def substituteTypeVariables(mapping: Map[Name, Type[Annotations]]): Type[Annotations] = self.caseValue match {
  //   case TypeCase.VariableCase(name) =>
  //     mapping.getOrElse(name, self)
  //   case TypeCase.ExtensibleRecordCase(name, fields) =>
  //     TypeCase.ExtensibleRecordCase(name, fields.map(_.substituteTypeVariables(mapping)))
  //   case TypeCase.FieldCase(name, fieldType) =>
  //     TypeCase.FieldCase(name, fieldType.substituteTypeVariables(mapping))
  //   case TypeCase.FunctionCase(paramTypes, returnType) =>
  //     TypeCase.FunctionCase(paramTypes.map(_.substituteTypeVariables(mapping)), returnType.substituteTypeVariables(mapping))
  //   case TypeCase.RecordCase(fields) =>
  //     TypeCase.RecordCase(fields.map(_.substituteTypeVariables(mapping)))
  //   case TypeCase.ReferenceCase(fqName, typeParams) =>
  //     TypeCase.ReferenceCase(fqName, typeParams.map(_.substituteTypeVariables(mapping)))
  //   case TypeCase.TupleCase(elementTypes) =>
  //     TypeCase.TupleCase(elementTypes.map(_.substituteTypeVariables(mapping)))
  //   case TypeCase.UnitCase =>
  //     TypeCase.UnitCase
  // }

  def satisfiesCaseOf(check: PartialFunction[Type[Attributes], Boolean]): Boolean =
    check.lift(self).getOrElse(false)
  //
  //    override def toString: String = fold[String] {
  //      case c @ Type.ExtensibleRecord(_, _) =>
  //        s"{ ${c.name.toCamel} | ${c.fields.mkString(", ")} }"
  //       Type.Function(paramTypes, returnType) =>
  //        paramTypes
  //          .map(_.toString)
  //          .mkString("(", ",", ")")
  //          .concat(" -> " + returnType.toString)
  //       Type.Record(fields)              => fields.mkString("{ ", ", ", " }")
  //       Type.Reference(name, typeParams) => s"${name.toString} ${typeParams.mkString(" ")}"
  //       Type.Tuple(elementTypes)         => elementTypes.mkString("(", ", ", ")")
  //       Type.Unit                        => "()"
  //       Type.Variable(name)              => name.toCamel
  //    }
}

private[ir] object Type extends TypeModuleSyntax {

  final case class ExtensibleRecord[+Attributes](
      attributes: Attributes,
      name: Name,
      fields: Chunk[Field[Type[Attributes]]]
  ) extends Type[Attributes]
  final case class Function[+Attributes](
      attributes: Attributes,
      paramTypes: Chunk[Type[Attributes]],
      returnType: Type[Attributes]
  ) extends Type[Attributes]
  final case class Record[+Attributes](attributes: Attributes, fields: Chunk[Field[Type[Attributes]]])
      extends Type[Attributes]
  final case class Reference[+Attributes](
      attributes: Attributes,
      typeName: FQName,
      typeParams: Chunk[Type[Attributes]]
  ) extends Type[Attributes]
  final case class Tuple[+Attributes](attributes: Attributes, elementTypes: Chunk[Type[Attributes]])
      extends Type[Attributes]
  final case class Unit[+Attributes](attributes: Attributes) extends Type[Attributes]

  final case class Variable[+Attributes](attributes: Attributes, name: Name) extends Type[Attributes]
  object Variable {
    def apply(name: Name): Variable[scala.Unit] = Variable((), name)
  }

}
