package zio.morphir.ir.types

import zio.Chunk
import zio.morphir.ir.{Documented, Name}

sealed trait Specification[+Attributes] { self =>
  import Specification._

  def ??(doc: String): Documented[Specification[Attributes]] =
    Documented(doc, self)

  // def map[Annotations0 >: Annotations](f: Annotations => Annotations0): Specification[Annotations0] = self match {
  //   case c @ TypeAliasSpecification(_, _, _) =>
  //     TypeAliasSpecification[Annotations0](c.typeParams, c.expr.map(f), c.annotations.map(f))
  //   case c @ OpaqueTypeSpecification(_, _) =>
  //     OpaqueTypeSpecification[Annotations0](c.typeParams, c.annotations.map(f))
  //   case c @ CustomTypeSpecification(_, _, _) =>
  //     CustomTypeSpecification[Annotations0](c.typeParams, c.ctors.map(f), c.annotations.map(f))
  // }
  def eraseAttributes: Specification[Any] = self match {
    case c @ TypeAliasSpecification(_, _) =>
      TypeAliasSpecification(c.typeParams, c.expr.eraseAttributes)
    case c @ OpaqueTypeSpecification(_) =>
      OpaqueTypeSpecification(c.typeParams)
    case c @ CustomTypeSpecification(_, _) =>
      CustomTypeSpecification(c.typeParams, c.ctors.eraseAttributes)
  }
}

object Specification {
  final case class TypeAliasSpecification[+Attributes](
      typeParams: Chunk[Name],
      expr: Type[Attributes]
  ) extends Specification[Attributes]

  final case class OpaqueTypeSpecification(typeParams: Chunk[Name]) extends Specification[Nothing]

  object OpaqueTypeSpecification {
    def apply(typeParams: String*): OpaqueTypeSpecification =
      OpaqueTypeSpecification(Chunk.fromIterable(typeParams.map(Name.fromString)))
  }

  final case class CustomTypeSpecification[+Attributes](
      typeParams: Chunk[Name],
      ctors: Constructors[Attributes]
  ) extends Specification[Attributes]

  object CustomTypeSpecification {
    def fromCtors[Attributes](
        ctor: (String, Iterable[(String, Type[Attributes])]),
        ctors: (String, Iterable[(String, Type[Attributes])])*
    ): CustomTypeSpecification[Attributes] = {
      val allCtors = (ctor +: ctors).map { case (name, args) =>
        (
          Name.fromString(name),
          Chunk.fromIterable(args.map { case (name, tpe) =>
            (Name.fromString(name), tpe)
          })
        )
      }.toMap
      CustomTypeSpecification(Chunk.empty, Constructors(allCtors))
    }

    def mkEnum(case1: String, otherCases: String*): CustomTypeSpecification[Any] =
      CustomTypeSpecification(Chunk.empty, Constructors.forEnum(case1, otherCases: _*))
  }

  type UCustomTypeSpecification = CustomTypeSpecification[Any]
  val UCustomTypeSpecification: CustomTypeSpecification.type = CustomTypeSpecification
}
