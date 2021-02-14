package morphir.ir

import zio.ZIO

/**
 * In look at how we can potentially model the Morphir IR, research has shown that recursIt woulkd
 */
object recursions {
//  type Type attribution
//  = Variable a Name
//  | Reference a FQName (List (Type a))
//  | Tuple a (List (Type a))
//  | Record a (List (Field a))
//  | ExtensibleRecord a Name (List (Field a))
//  | Function a (Type a) (Type a)
//  | Unit a
//

//
//  type alias Field a =
//    { name : Name
//      , tpe : Type a
//    }
//

  type Name   = String
  type FQNAme = String

  final case class Field[+Self](name: Name, value: Self) {
    def map[Self2](fn: Self => Self2): Field[Self2]                              = Field(name, fn(value))
    def mapM[R, E, Self2](fn: Self => ZIO[R, E, Self2]): ZIO[R, E, Field[Self2]] = fn(value).map(Field(name, _))
  }

  sealed trait TypeCase[+Self, +Attrib] {
    def map[Self2](fn: Self => Self2): TypeCase[Self2, Attrib]
    def mapAttrib[Attrib2](f: Attrib => Attrib2): TypeCase[Self, Attrib2]
    def mapM[R, E, Self2](fn: Self => ZIO[R, E, Self2]): ZIO[R, E, TypeCase[Self2, Attrib]]
  }
  object TypeCase {
    final case class Variable[Attrib](a: Attrib) extends TypeCase[Nothing, Attrib] { self =>
      def map[Self2](fn: Nothing => Self2): TypeCase[Self2, Attrib] = self

      def mapAttrib[Attrib2](f: Attrib => Attrib2): TypeCase[Nothing, Attrib2] = copy(a = f(a))

      def mapM[R, E, Self2](fn: Nothing => ZIO[R, E, Self2]): ZIO[R, E, TypeCase[Self2, Attrib]] = ZIO.succeed(self)
    }
    final case class Reference[Self, Attrib](attribute: Attrib, name: FQName, types: List[Self])
        extends TypeCase[Self, Attrib] {
      def map[Self2](fn: Self => Self2): TypeCase[Self2, Attrib] = Reference(attribute, name, types.map(fn))

      def mapAttrib[Attrib2](f: Attrib => Attrib2): TypeCase[Self, Attrib2] = copy(attribute = f(attribute))

      def mapM[R, E, Self2](fn: Self => ZIO[R, E, Self2]): ZIO[R, E, TypeCase[Self2, Attrib]] =
        ZIO.foreach(types)(fn).map(Reference(attribute, name, _))
    }
    final case class Tuple[Self, Attrib](attribute: Attrib, types: List[Self]) extends TypeCase[Self, Attrib] {
      def map[Self2](fn: Self => Self2): TypeCase[Self2, Attrib] = Tuple(attribute, types.map(fn))

      def mapAttrib[Attrib2](f: Attrib => Attrib2): TypeCase[Self, Attrib2] = copy(attribute = f(attribute))

      def mapM[R, E, Self2](fn: Self => ZIO[R, E, Self2]): ZIO[R, E, TypeCase[Self2, Attrib]] =
        ZIO.foreach(types)(fn).map(Tuple(attribute, _))
    }
    final case class Record[Self, Attrib](attribute: Attrib, fields: List[Field[Self]]) extends TypeCase[Self, Attrib] {
      def map[Self2](fn: Self => Self2): TypeCase[Self2, Attrib]            = Record(attribute, fields.map(_.map(fn)))
      def mapAttrib[Attrib2](f: Attrib => Attrib2): TypeCase[Self, Attrib2] = copy(attribute = f(attribute))

      def mapM[R, E, Self2](fn: Self => ZIO[R, E, Self2]): ZIO[R, E, TypeCase[Self2, Attrib]] =
        ZIO.foreach(fields)(_.mapM(fn)).map(Record(attribute, _))
    }
    final case class ExtensibleRecord[Self, Attrib](attribute: Attrib, name: Name, fields: List[Field[Self]])
        extends TypeCase[Self, Attrib] {
      def map[Self2](fn: Self => Self2): TypeCase[Self2, Attrib] =
        ExtensibleRecord(attribute, name, fields.map(_.map(fn)))

      def mapAttrib[Attrib2](f: Attrib => Attrib2): TypeCase[Self, Attrib2] = copy(attribute = f(attribute))

      def mapM[R, E, Self2](fn: Self => ZIO[R, E, Self2]): ZIO[R, E, TypeCase[Self2, Attrib]] =
        ZIO.foreach(fields)(_.mapM(fn)).map(ExtensibleRecord(attribute, name, _))
    }
    final case class Function[Self, Attrib](attribute: Attrib, input: Self, output: Self)
        extends TypeCase[Self, Attrib] {
      def map[Self2](fn: Self => Self2): TypeCase[Self2, Attrib]            = Function(attribute, fn(input), fn(output))
      def mapAttrib[Attrib2](f: Attrib => Attrib2): TypeCase[Self, Attrib2] = copy(attribute = f(attribute))

      def mapM[R, E, Self2](fn: Self => ZIO[R, E, Self2]): ZIO[R, E, TypeCase[Self2, Attrib]] =
        ZIO.mapN(fn(input), fn(output)) { case (input1, output1) =>
          Function(attribute, input1, output1)
        }
    }
    final case class Unit[+Attrib](attribute: Attrib) extends TypeCase[Nothing, Attrib] { self =>
      def map[Self2](fn: Nothing => Self2): TypeCase[Self2, Attrib]            = self
      def mapAttrib[Attrib2](f: Attrib => Attrib2): TypeCase[Nothing, Attrib2] = copy(attribute = f(attribute))

      def mapM[R, E, Self2](fn: Nothing => ZIO[R, E, Self2]): ZIO[R, E, TypeCase[Self2, Attrib]] = ZIO.succeed(self)
    }
  }

  final case class Type[+Attrib](value: TypeCase[Type[Attrib], Attrib]) { self =>

    //NOTE: Useful for bottom up type inference for example
    def annotate[Attrib2](f: TypeCase[Type[Attrib2], Attrib] => Attrib2): Type[Attrib2] =
      transformUpRecursive[Attrib2](value => value.mapAttrib(_ => f(value)))

    def fold[Z](f: TypeCase[Z, Attrib] => Z): Z =
      f(value.map(_.fold(f)))

    def foldM[R, E, Z](f: TypeCase[Z, Attrib] => ZIO[R, E, Z]): ZIO[R, E, Z] =
      value.mapM(_.foldM(f)).flatMap(f)

    /**
     * Transform the whole tree.
     * Top down
     */
    def transformDown[Attrib2](f: TypeMapper[Attrib, Attrib2]): Type[Attrib2] =
      Type(f(value).map(_.transformDown(f)))

    /**
     * Bottom up
     */
    def transformUp[Attrib2](f: TypeMapper[Attrib, Attrib2]): Type[Attrib2] =
      Type(f(value.map(_.transformUp(f))))

    def transformDownRecursive[Attrib1 >: Attrib, Attrib2](
      f: TypeCase[Type[Attrib1], Attrib1] => TypeCase[Type[Attrib1], Attrib2]
    ): Type[Attrib2] =
      Type(f(value).map(_.transformDownRecursive(f)))

    def transformUpRecursive[Attrib2](
      f: TypeCase[Type[Attrib2], Attrib] => TypeCase[Type[Attrib2], Attrib2]
    ): Type[Attrib2] =
      Type(f(value.map(_.transformUpRecursive(f))))

  }

  object Type {
    def unfold[Z, Attrib](initial: Z)(f: Z => TypeCase[Z, Attrib]): Type[Attrib] =
      Type(f(initial).map(unfold(_)(f)))
  }

  final case class TypeMapperRec[A, B, C, D](fn: TypeCase[Type[A], B] => TypeCase[Type[C], D])
      extends Function[TypeCase[Type[A], B], TypeCase[Type[C], D]] {
    override def apply(v1: TypeCase[Type[A], B]): TypeCase[Type[C], D] = fn(v1)

  }

  trait TypeMapper[-AttribIn, +AttribOut] {
    def apply[Self](value: TypeCase[Self, AttribIn]): TypeCase[Self, AttribOut]
  }
}
