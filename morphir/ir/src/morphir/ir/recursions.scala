package morphir.ir

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

  final case class Field[+Self](name: Name, value: Self)

  sealed trait TypeCase[+Self, +Attrib] {
    def map[Self2](fn: Self => Self2): TypeCase[Self2, Attrib] = ???
  }
  object TypeCase {
    final case class Variable[Attrib](a: Attrib) extends TypeCase[Nothing, Attrib]
    final case class Reference[Self, Attrib](attribute: Attrib, name: FQName, types: List[Self])
        extends TypeCase[Self, Attrib]
    final case class Tuple[Self, Attrib](attribute: Attrib, types: List[Self])           extends TypeCase[Self, Attrib]
    final case class Record[Self, Attrib](attributes: Attrib, fields: List[Field[Self]]) extends TypeCase[Self, Attrib]
    final case class ExtensibleRecord[Self, Attrib](attributes: Attrib, name: Name, fields: List[Field[Self]])
        extends TypeCase[Self, Attrib]
    final case class Function[Self, Attrib](attribute: Attrib, input: Self, output: Self) extends TypeCase[Self, Attrib]
    final case class Unit[+Attrib](attribute: Attrib)                                     extends TypeCase[Nothing, Attrib]
  }

  final case class Type[+Attrib](value: TypeCase[Type[Attrib], Attrib]) { self =>
    def fold[Z](f: TypeCase[Z, Attrib] => Z): Z =
      f(value.map(_.fold(f)))

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

  }

  object Type {
    def unfold[Z, Attrib](initial: Z)(f: Z => TypeCase[Z, Attrib]): Type[Attrib] =
      Type(f(initial).map(unfold(_)(f)))
  }

  trait TypeMapper[-AttribIn, +AttribOut] {
    def apply[Self](value: TypeCase[Self, AttribIn]): TypeCase[Self, AttribOut]
  }
}
