package zio.morphir.ir.types

import zio.Chunk
import zio.morphir.ir.{Documented, FQName, Name}
import zio.morphir.syntax.TypeModuleSyntax
import zio.prelude._

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

  def fold[Z: Associative](f: Type[Attributes] => Z): Z = {
    @tailrec
    def loop(types: List[Type[Attributes]], acc: Z): Z = types match {
      case (tpe @ Type.ExtensibleRecord(_, _, _)) :: tail =>
        loop(tpe.fields.map(_.fieldType).toList ++ tail, f(tpe) <> acc)
      case (tpe @ Type.Function(_, _, _)) :: tail =>
        loop(tpe.paramTypes.toList ++ (tpe.returnType :: tail), f(tpe) <> acc)
      case (tpe @ Type.Record(_, _)) :: tail       => loop(tpe.fields.map(_.fieldType).toList ++ tail, f(tpe) <> acc)
      case (tpe @ Type.Reference(_, _, _)) :: tail => loop(tpe.typeParams.toList ++ tail, f(tpe) <> acc)
      case (tpe @ Type.Tuple(_, elements)) :: tail => loop(elements.toList ++ tail, f(tpe) <> acc)
      case Type.Variable(_, _) :: tail             => loop(tail, acc)
      case Type.Unit(_) :: tail                    => loop(tail, acc)
      case Nil                                     => acc
    }
    loop(List(self), f(self))
  }

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
  //
  //    def foldM[F[+_]: AssociativeFlatten: Covariant: IdentityBoth, Z](f: TypeCase[Z] => F[Z]): F[Z] =
  //      fold[F[Z]](_.flip.flatMap(f))
  //
  //    def foldPure[W, S, R, E, Z](f: TypeCase[Z] => ZPure[W, S, S, R, E, Z]): ZPure[W, S, S, R, E, Z] =
  //      foldM(f)
  //
  //    def transformDown[Annotations0 >: Attributes](
  //        f: Type[Annotations0] => Type[Annotations0]
  //    ): Type[Annotations0] = {
  //      def loop(recursive: Type[Annotations0]): Type[Attributes] =
  //        Type(f(recursive).caseValue.map(loop), attributes)
  //      loop(self)
  //    }
  //
  //    def foldZIO[R, E, Z](f: TypeCase[Z] => ZIO[R, E, Z]): ZIO[R, E, Z] =
  //      foldM(f)
  //
  //    def foldRecursive[Z](f: TypeCase[(Type[Attributes], Z)] => Z): Z =
  //      f(caseValue.map(recursive => recursive -> recursive.foldRecursive(f)))
  //
  def foldUp[Z](z: Z)(f: (Z, Type[Attributes]) => Z): Z =
    f(self.foldLeft(z)((z, recursive) => recursive.foldUp(z)(f)), self)

  def foldUpSome[Z](z: Z)(pf: PartialFunction[(Z, Type[Attributes]), Z]): Z =
    foldUp(z)((z, recursive) => pf.lift(z -> recursive).getOrElse(z))

  // TODO: See if we can refactor to be stack safe/ tail recursive
  def mapAttributes[Attributes2](f: Attributes => Attributes2): Type[Attributes2] = self match {
    case Type.ExtensibleRecord(attributes, name, fields) =>
      Type.ExtensibleRecord(f(attributes), name, fields.map(_.mapAttributes(f)))
    case Type.Function(attributes, paramTypes, returnType) =>
      Type.Function(f(attributes), paramTypes.map(_.mapAttributes(f)), returnType.mapAttributes(f))
    case Type.Record(attributes, fields) => Type.Record(f(attributes), fields.map(_.mapAttributes(f)))
    case Type.Reference(attributes, typeName, typeParams) =>
      Type.Reference(f(attributes), typeName, typeParams.map(_.mapAttributes(f)))
    case Type.Tuple(attributes, elementTypes) => Type.Tuple(f(attributes), elementTypes.map(_.mapAttributes(f)))
    case Type.Unit(attributes)                => Type.Unit(f(attributes))
    case Type.Variable(attributes, name)      => Type.Variable(f(attributes), name)
  }

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

object Type extends TypeModuleSyntax {

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
